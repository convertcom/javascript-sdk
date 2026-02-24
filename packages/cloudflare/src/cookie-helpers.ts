/*!
 * Convert JS SDK - Cloudflare Workers Utilities
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

const DEFAULT_COOKIE_NAME = 'convert_visitor_id';
const DEFAULT_MAX_AGE = 31536000; // 1 year in seconds

/**
 * Extract the visitor ID from a Workers Request cookie header.
 *
 * @param request - The incoming Workers Request
 * @param cookieName - Cookie name to read (default: 'convert_visitor_id')
 * @returns The visitor ID string, or null if not found
 */
export function getVisitorId(
  request: Request,
  cookieName = DEFAULT_COOKIE_NAME
): string | null {
  const cookie = request.headers.get('Cookie') || '';
  const escaped = cookieName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  const match = cookie.match(new RegExp(`(?:^|;)\\s*${escaped}=([^;]+)`));
  return match ? decodeURIComponent(match[1]) : null;
}

/**
 * Append a Set-Cookie header for the visitor ID on the response.
 *
 * @param headers - The response Headers object to modify
 * @param visitorId - The visitor ID to persist
 * @param cookieName - Cookie name to set (default: 'convert_visitor_id')
 * @param maxAge - Cookie max-age in seconds (default: 31536000 = 1 year)
 */
export function setVisitorIdCookie(
  headers: Headers,
  visitorId: string,
  cookieName = DEFAULT_COOKIE_NAME,
  maxAge = DEFAULT_MAX_AGE
): void {
  headers.append(
    'Set-Cookie',
    `${cookieName}=${encodeURIComponent(visitorId)}; Path=/; Max-Age=${maxAge}; SameSite=Lax; Secure; HttpOnly`
  );
}

/**
 * Generate a new unique visitor ID using crypto.randomUUID().
 * Available in all Cloudflare Workers runtimes.
 */
export function generateVisitorId(): string {
  return crypto.randomUUID();
}
