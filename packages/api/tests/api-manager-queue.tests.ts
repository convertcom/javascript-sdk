/****************************************************************************
 * Copyright 2026, Convert Insights, Inc. and contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************/
import 'mocha';
import {expect} from 'chai';
import {ApiManager} from '../src/api-manager';
import {
  Config,
  TrackingEvent,
  VisitorTrackingEvents
} from '@convertcom/js-sdk-types';

const bucketing: VisitorTrackingEvents = {
  eventType: 'bucketing',
  data: {experienceId: 'experience', variationId: 'variation'}
};
const conversion: VisitorTrackingEvents = {
  eventType: 'conversion',
  data: {goalId: 'signup'}
};
const nextTurn = () => new Promise<void>((resolve) => setImmediate(resolve));

describe('ApiManager tracking queue concurrency', function () {
  let api: ApiManager;
  let originalFetch: typeof fetch;
  let sent: TrackingEvent[];
  let respond: (status: number) => void;

  beforeEach(async function () {
    sent = [];
    originalFetch = globalThis.fetch;
    // Capture the real HttpClient's body and hold the response open.
    globalThis.fetch = ((_url, options) => {
      sent.push(JSON.parse(options.body as string));
      return new Promise<Response>((resolve) => {
        respond = (status) =>
          resolve({status, json: async () => ({})} as Response);
      });
    }) as typeof fetch;
    api = new ApiManager({
      sdkKey: 'test-key',
      data: {account_id: 'account', project: {id: 'project'}},
      api: {endpoint: {track: 'https://collector.invalid'}},
      network: {tracking: true},
      events: {batch_size: 2, release_interval: 10000}
    } as Config);
    api.enqueue('first', bucketing, {country: 'TH'});
    api.enqueue('second', bucketing);
    await nextTurn();
  });

  afterEach(async function () {
    api.disableTracking();
    respond(200);
    await nextTurn();
    api.stopQueue();
    globalThis.fetch = originalFetch;
  });

  it('sends a visitor enqueued while the previous batch is in flight', async function () {
    api.enqueue('late', bucketing);
    respond(200);
    await nextTurn();

    expect(
      sent.map((batch) => batch.visitors.map((v) => v.visitorId))
    ).to.deep.equal([['first', 'second'], ['late']]);
  });

  it('sends a late conversion for a visitor in the previous batch', async function () {
    api.enqueue('first', conversion, {country: 'JP'});
    respond(200);
    await nextTurn();

    expect(sent).to.have.lengthOf(2);
    expect(sent[0].visitors[0].events).to.deep.equal([bucketing]);
    expect(sent[1].visitors).to.deep.equal([
      {visitorId: 'first', events: [conversion], segments: {country: 'JP'}}
    ]);
  });

  it('coalesces flushes and retries the failed batch before new events', async function () {
    const flushing = api.releaseQueue('manual');
    api.enqueue('first', conversion, {country: 'JP'});
    expect(api.releaseQueue('manual')).to.equal(flushing);
    expect(sent).to.have.lengthOf(1);
    respond(500);
    await flushing;

    const retrying = api.releaseQueue('manual');
    await nextTurn();
    expect(sent[1]).to.deep.equal(sent[0]);
    respond(200);
    await nextTurn();
    expect(sent[2].visitors).to.deep.equal([
      {visitorId: 'first', events: [conversion], segments: {country: 'JP'}}
    ]);
    respond(200);
    await retrying;
    expect(sent).to.have.lengthOf(3);
  });
});
