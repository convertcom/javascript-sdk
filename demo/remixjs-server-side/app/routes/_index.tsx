import { useEffect } from "react";
import type { MetaFunction, LoaderFunction, ActionFunction } from "@remix-run/node";
import { json } from "@remix-run/node";
import { useLoaderData, useFetcher } from "@remix-run/react";
import { getConvertContext } from "../api/convert.server";
import type { BucketedVariation, ConversionAttributes } from "@convertcom/js-sdk";
import { RuleError, BucketingError } from "@convertcom/js-sdk";

interface ConvertLoaderData {
  convertVariations?: (BucketedVariation | RuleError | BucketingError)[];
}

export const loader: LoaderFunction = async ({ request }) => {
  try {
    const { context, setCookieHeader } = await getConvertContext(request);
    const convertVariations = context.runExperiences({
      locationProperties: { location: "global" },
    });

    const headers = new Headers();
    if (setCookieHeader) {
      headers.append("Set-Cookie", setCookieHeader);
    }

    return json({ convertVariations }, { headers });
  } catch (error) {
    console.error("Error in loader:", error);
    throw new Response("Server Error", { status: 500 });
  }
};

export const action: ActionFunction = async ({ request }) => {
  try {
    const { context, setCookieHeader } = await getConvertContext(request);

    const form = await request.formData();
    const actionType = form.get("action");
    if (actionType === "trackConversion") {
      const goalId = form.get("goalId") as string | null;
      const goalRuleValue = form.get("goalRule");
      const goalDataValue = form.get("goalData");

      const goalRule = goalRuleValue
        ? JSON.parse(goalRuleValue.toString())
        : {};
      const goalData = goalDataValue
        ? JSON.parse(goalDataValue.toString())
        : [];
      const goalAttributes: ConversionAttributes = {};
      if (Object.keys(goalRule).length) {
        goalAttributes.ruleData = goalRule;
      }
      if (goalData.length) {
        goalAttributes.conversionData = goalData;
      }
      if (goalId) {
        context.trackConversion(goalId, goalAttributes);
      }
    }

    const headers = new Headers();
    if (setCookieHeader) {
      headers.append("Set-Cookie", setCookieHeader);
    }

    return json(null, { headers });
  } catch (error) {
    console.error("Error in action:", error);
    throw new Response("Server Error", { status: 500 });
  }
};

export const meta: MetaFunction = () => {
  return [
    { title: "New Remix App" },
    { name: "description", content: "Welcome to Remix!" },
  ];
};

export default function Index() {
  const { convertVariations } = useLoaderData<ConvertLoaderData>();

  const fetcher = useFetcher();

  useEffect(() => {
    console.log("convertVariations:", convertVariations);
  }, [convertVariations]);

  interface SubmitPayload {
    action: string;
    goalId: string;
    goalRule?: Record<string, unknown>;
    goalData?: Array<{ key: string; value: number }>;
  }

  // Define handleSubmit before handleClick to avoid hoisting issues
  function handleSubmit(
    fetcher: ReturnType<typeof useFetcher>,
    payload: SubmitPayload
  ) {
    const formData = new FormData();
    Object.entries(payload).forEach(([key, value]) => {
      formData.append(
        key,
        typeof value === "object" ? JSON.stringify(value) : String(value)
      );
    });
    fetcher.submit(formData, {
      method: "post",
    });
  }

  const handleClick = (e: React.MouseEvent<HTMLButtonElement>) => {
    e.preventDefault();
    const payload: SubmitPayload = {
      action: "trackConversion",
      goalId: "add-to-cart",
      // goalRule: { action: "buy" }, // Uncomment to pass goal rules
      // goalData: [
      //   { key: "amount", value: 10.3 },
      //   { key: "productsCount", value: 2 },
      // ], // Uncomment to pass revenue data
    };
    handleSubmit(fetcher, payload);
  };

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

      <button onClick={handleClick}>Trigger Goal</button>
    </div>
  );
}
