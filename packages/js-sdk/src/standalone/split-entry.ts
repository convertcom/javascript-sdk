import {VariationChangeType} from '@convertcom/js-sdk-enums';
import {
  BucketedVariation,
  ConfigExperience
} from '@convertcom/js-sdk-types';
import {
  ConvertStandaloneRuntime,
  ensureConvertWindow,
  initializeVisitorRuntime
} from './runtime';

const SPLIT_TEST_COOKIE = '_conv_sptest';

type SplitCookieState = {
  destinationUrl: string;
  experienceId?: string;
  experienceKey: string;
  fromHash: string;
  timestamp: number;
  toHash: string;
  variationId?: string;
  variationKey?: string;
};

const hashLocation = (value: string): string => {
  let hash = 5381;
  for (let i = 0; i < value.length; i++) {
    hash = (hash * 33) ^ value.charCodeAt(i);
  }
  return (hash >>> 0).toString(16);
};

const readSplitCookie = (): SplitCookieState | null => {
  if (typeof document === 'undefined') return null;
  const cookie = document.cookie
    .split('; ')
    .find((item) => item.startsWith(`${SPLIT_TEST_COOKIE}=`));
  if (!cookie) return null;
  try {
    return JSON.parse(decodeURIComponent(cookie.split('=').slice(1).join('=')));
  } catch {
    return null;
  }
};

const writeSplitCookie = (payload: SplitCookieState): void => {
  if (typeof document === 'undefined') return;
  document.cookie = `${SPLIT_TEST_COOKIE}=${encodeURIComponent(
    JSON.stringify(payload)
  )}; path=/; SameSite=Lax`;
};

const clearSplitCookie = (): void => {
  if (typeof document === 'undefined') return;
  document.cookie = `${SPLIT_TEST_COOKIE}=; Max-Age=0; path=/; SameSite=Lax`;
};

const isBucketedVariation = (value: unknown): value is BucketedVariation =>
  !!value && typeof value === 'object' && 'experienceKey' in (value as object);

const normalizeLocationIds = (value: unknown): Array<string> =>
  Array.isArray(value) ? value.map((item) => String(item)) : [];

const getSplitExperiences = (
  runtime: ConvertStandaloneRuntime,
  activeLocations: Array<string>
): Array<ConfigExperience> =>
  runtime.experienceManager
    .getList()
    .filter(({type}) => type === 'split_url')
    .filter(
      (experience) =>
        !Array.isArray(experience.locations) ||
        !experience.locations.length ||
        experience.locations.some((locationId) =>
          activeLocations.includes(String(locationId))
        )
    );

const getRedirectUrl = (
  currentUrl: string,
  experience: ConfigExperience,
  variation: BucketedVariation
): string | null => {
  const redirectChange = variation.changes?.find(
    ({type}) => type === VariationChangeType.DEFAULT_REDIRECT
  );
  const redirectData = redirectChange?.data as Record<string, any>;
  if (!redirectData?.variation_pattern) return null;

  const originalPattern = String(redirectData.original_pattern || '');
  const variationPattern = String(redirectData.variation_pattern || '');
  const caseSensitive = Boolean(redirectData.case_sensitive);
  const regexSupport = Boolean(
    experience?.settings?.split_url_settings?.split_regex_support
  );

  let nextUrl = variationPattern;
  if (originalPattern) {
    if (regexSupport) {
      const flags = caseSensitive ? '' : 'i';
      const pattern = new RegExp(originalPattern, flags);
      if (!pattern.test(currentUrl)) return null;
      nextUrl = currentUrl.replace(pattern, variationPattern);
    } else if (caseSensitive) {
      if (!currentUrl.includes(originalPattern)) return null;
      nextUrl = currentUrl.replace(originalPattern, variationPattern);
    } else {
      const loweredUrl = currentUrl.toLowerCase();
      const loweredPattern = originalPattern.toLowerCase();
      const index = loweredUrl.indexOf(loweredPattern);
      if (index === -1) return null;
      nextUrl =
        currentUrl.slice(0, index) +
        variationPattern +
        currentUrl.slice(index + originalPattern.length);
    }
  }

  try {
    return new URL(nextUrl, currentUrl).toString();
  } catch {
    return nextUrl || null;
  }
};

const registerHelpers = (convert: Record<string, any>): void => {
  convert.splitTests = convert.splitTests || {};
  convert.redirect =
    convert.redirect ||
    ((url: string, replace = true) => {
      if (typeof window === 'undefined' || !url) return;
      if (replace) {
        window.location.replace(url);
      } else {
        window.location.assign(url);
      }
    });
  convert.refresh =
    convert.refresh ||
    (() => {
      if (typeof window === 'undefined') return;
      window.location.reload();
    });
};

const processDestinationFlow = (convert: Record<string, any>): boolean => {
  const payload = readSplitCookie();
  if (!payload || typeof window === 'undefined') return false;

  clearSplitCookie();

  const currentHash = hashLocation(window.location.href);
  if (
    !payload.fromHash ||
    !payload.toHash ||
    payload.fromHash === currentHash ||
    payload.toHash !== currentHash
  ) {
    return false;
  }

  convert.splitTests[payload.experienceKey] = payload;
  return true;
};

const processOriginFlow = (
  convert: Record<string, any>,
  runtime: ConvertStandaloneRuntime
): void => {
  if (typeof window === 'undefined') return;

  const visitorId = convert.visitor?.id;
  if (!visitorId) return;

  const activeLocations = normalizeLocationIds(convert.activeLocations);
  const visitorAttributes =
    convert.visitor?.attributes &&
    typeof convert.visitor.attributes === 'object' &&
    !Array.isArray(convert.visitor.attributes)
      ? convert.visitor.attributes
      : {};
  const currentUrl = window.location.href;
  const currentHash = hashLocation(currentUrl);

  for (const experience of getSplitExperiences(runtime, activeLocations)) {
    if (!experience?.key) continue;

    const bucketedVariation = runtime.experienceManager.selectVariation(
      visitorId,
      experience.key,
      {
        locationProperties: convert.ruleData,
        visitorProperties: visitorAttributes,
        enableTracking: false
      }
    );

    if (!isBucketedVariation(bucketedVariation)) continue;

    const destinationUrl = getRedirectUrl(
      currentUrl,
      experience,
      bucketedVariation
    );
    if (!destinationUrl) continue;

    const destinationHash = hashLocation(destinationUrl);
    if (destinationHash === currentHash) continue;

    const splitState: SplitCookieState = {
      destinationUrl,
      experienceId: experience.id,
      experienceKey: experience.key,
      fromHash: currentHash,
      timestamp: Date.now(),
      toHash: destinationHash,
      variationId: bucketedVariation.id,
      variationKey: bucketedVariation.key
    };

    writeSplitCookie(splitState);
    convert.splitTests[experience.key] = splitState;
    convert.redirect(destinationUrl);
    return;
  }
};

export const runSplitEntry = async () => {
  const runtime = await initializeVisitorRuntime();
  if (!runtime || typeof window === 'undefined') return null;

  const convert = ensureConvertWindow();
  registerHelpers(convert);

  if (processDestinationFlow(convert)) {
    return convert.splitTests;
  }

  processOriginFlow(convert, runtime);
  return convert.splitTests;
};

void runSplitEntry();
