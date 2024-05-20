import {expect} from 'chai';

export function getVariationsAcrossAllExperiences(
  {accountId, projectId, context, server},
  done
) {
  const variationIds = ['100299456', '100299457', '100299460', '100299461'];
  const variations = context.runExperiences({
    locationProperties: {url: 'https://convert.com/'},
    visitorProperties: {
      varName3: 'something'
    }
  });
  if (server) {
    server.on('request', (request, res) => {
      if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
        request.on('end', () => {
          expect(variations).to.be.an('array').that.have.length(2);
          variations.forEach((variation) =>
            expect(variation)
              .to.be.an('object')
              .that.have.keys(
                'experienceId',
                'experienceKey',
                'experienceName',
                'bucketingAllocation',
                'id',
                'key',
                'name',
                'status',
                'changes',
                'is_baseline',
                'traffic_allocation'
              )
          );
          const selectedVariations = variations.map(({id}) => id);
          expect(variationIds).to.include.deep.members(selectedVariations);
          done();
        });
      }
      res.writeHead(200, {'Content-Type': 'application/json'});
      res.end('{}');
    });
  } else if (done) {
    done();
  }
}

export function getSingleFeatureWithStatus(
  {accountId, projectId, featureId, context, server},
  done
) {
  const featureKey = 'feature-2';
  const feature = context.runFeature(featureKey, {
    locationProperties: {url: 'https://convert.com/'},
    visitorProperties: {
      varName3: 'something'
    }
  });
  if (server) {
    server.on('request', (request, res) => {
      if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
        request.on('end', () => {
          expect(feature)
            .to.be.an('object')
            .that.have.keys(
              'experienceId',
              'experienceKey',
              'experienceName',
              'id',
              'key',
              'name',
              'status',
              'variables'
            );
          expect(feature.id).to.equal(featureId);
          done();
        });
      }
      res.writeHead(200, {'Content-Type': 'application/json'});
      res.end('{}');
    });
  } else if (done) {
    done();
  }
}

export function getMultipleFeatureWithStatus(
  {accountId, projectId, context, server},
  done
) {
  const featureKey = 'feature-1';
  const featureIds = ['10024', '10025'];
  const features = context.runFeature(featureKey, {
    locationProperties: {url: 'https://convert.com/'},
    visitorProperties: {
      varName3: 'something'
    }
  });
  if (server) {
    server.on('request', (request, res) => {
      if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
        request.on('end', () => {
          expect(features).to.be.an('array').that.have.length(2);
          features.forEach((feature) =>
            expect(feature)
              .to.be.an('object')
              .that.have.keys(
                'experienceId',
                'experienceKey',
                'experienceName',
                'id',
                'key',
                'name',
                'status',
                'variables'
              )
          );
          const selectedFeatures = features.map(({id}) => id);
          expect(featureIds).to.include.deep.members(selectedFeatures);
          done();
        });
      }
      res.writeHead(200, {'Content-Type': 'application/json'});
      res.end('{}');
    });
  } else if (done) {
    done();
  }
}

export function getFeaturesWithStatuses(
  {accountId, projectId, context, server},
  done
) {
  const featureIds = ['10024', '10025', '10026'];
  const features = context.runFeatures({
    locationProperties: {url: 'https://convert.com/'},
    visitorProperties: {
      varName3: 'something'
    }
  });
  if (server) {
    server.on('request', (request, res) => {
      if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
        request.on('end', () => {
          expect(features).to.be.an('array').that.have.length(4);
          features
            .filter(({status}) => status === 'enabled')
            .forEach((feature) =>
              expect(feature)
                .to.be.an('object')
                .that.have.keys(
                  'experienceId',
                  'experienceKey',
                  'experienceName',
                  'id',
                  'key',
                  'name',
                  'status',
                  'variables'
                )
            );
          features
            .filter(({status}) => status === 'disabled')
            .forEach((feature) =>
              expect(feature)
                .to.be.an('object')
                .that.have.keys('id', 'key', 'name', 'status')
            );
          const selectedFeatures = features.map(({id}) => id);
          expect(featureIds).to.include.deep.members(selectedFeatures);
          done();
        });
      }
      res.writeHead(200, {'Content-Type': 'application/json'});
      res.end('{}');
    });
  } else if (done) {
    done();
  }
}
