This is a [Next.js](https://nextjs.org) project bootstrapped with [`create-next-app`](https://nextjs.org/docs/app/api-reference/cli/create-next-app).

## Getting Started

First, run the development server:

```bash
npm run dev
# or
yarn dev
# or
pnpm dev
# or
bun dev
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.

You can start editing the page by modifying `app/page.tsx`. The page auto-updates as you edit the file.

This project uses [`next/font`](https://nextjs.org/docs/app/building-your-application/optimizing/fonts) to automatically optimize and load [Geist](https://vercel.com/font), a new font family for Vercel.

## Testing preview links

Force a specific variation to render — regardless of bucketing, audience/segment/location rules, environment, experience/variation status, or traffic allocation, and even for **draft/paused** experiences (the SDK fetches them via `?exp=`). No tracking events or visitor-state writes happen on a preview context (zero-trace).

**URL param format:** `?convert_preview={experienceId}.{variationId}` (dot-separated numeric ids — mirrors the web tracking script's force-param).

**Example:**

```
http://localhost:3005/?convert_preview=100123.200456
```

Replace `100123.200456` with a real experience id / variation id pair from your Convert project. The param is read server-side in `src/app/api/convert/route.js`, where the visitor's Convert context is created.

> The preview link is normally generated from the Convert UI's per-variation "Copy preview link". You can also construct it manually as `{experienceId}.{variationId}`.

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js) - your feedback and contributions are welcome!

## Deploy on Vercel

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out our [Next.js deployment documentation](https://nextjs.org/docs/app/building-your-application/deploying) for more details.
