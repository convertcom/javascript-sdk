import { useEffect } from "react";
import type { MetaFunction } from "@remix-run/node";
import { useConvertContext } from "../providers/Convert";

export const meta: MetaFunction = () => {
  return [
    { title: "New Remix App" },
    { name: "description", content: "Welcome to Remix!" },
  ];
};

export default function Index() {
  const convertContext = useConvertContext();

  useEffect(() => {
    try {
      if (convertContext) {
        const convertVariations = convertContext.runExperiences({
          locationProperties: { location: "global" },
        });
        console.log("convertVariations:", convertVariations);
      }
    } catch (error) {
      console.error("Error running experiences:", error);
    }
  }, [convertContext]);

  return (
    <div style={{ fontFamily: "system-ui, sans-serif", lineHeight: "1.8" }}>
      <h1>Welcome to Remix</h1>
      <ul>
        <li>
          <a
            target="_blank"
            href="https://remix.run/tutorials/blog"
            rel="noreferrer"
          >
            15m Quickstart Blog Tutorial
          </a>
        </li>
        <li>
          <a
            target="_blank"
            href="https://remix.run/tutorials/jokes"
            rel="noreferrer"
          >
            Deep Dive Jokes App Tutorial
          </a>
        </li>
        <li>
          <a target="_blank" href="https://remix.run/docs" rel="noreferrer">
            Remix Docs
          </a>
        </li>
      </ul>
    </div>
  );
}
