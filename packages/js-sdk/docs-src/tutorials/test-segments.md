# Testing User Segments

## Assumptions

You want to show a feature rollout to a specific segment of your visitors on the `homepage` view of your e-commerce app.

## Steps to Set Up

### Step 1: Create a Location

1. Open your Fullstack project at the web interface.
2. Create a location with the following details:
   - **Rule Type:** `generic_text_key_value`
   - **Match Type:** `matches`
   - **Key:** `location`
   - **Value:** `homepage`

### Step 2: Create an Audience (Segmentation)

1. Create an audience with the following details:
   - **Name:** `Halloween Segment`
   - **Type:** `segmentation`
   - **Key:** `halloween-segment`
   - **Rules:**
     - **Rule Type:** `generic_bool_key_value`
     - **Match Type:** `equals`
     - **Key:** `halloween`
     - **Value:** `true`

### Step 3: Create a Feature

1. Create a feature with the following details:
   - **Name:** `Halloween Bundles`
   - **Key:** `halloween-bundles`
   - **Variables:**
     - **Key:** `bundles`
     - **Type:** `json`

### Step 4: Create an Experience

1. Create an experience with one variation (without the original) linked to the above location, segmentation, and feature:
   - **Variation 1:**
     - **Traffic:** `100%`
     - **Key:** `with-halloween-bundles`
     - **Changes:**
       - **Type:** `fullStackFeature`
       - **Feature:** `Halloween Bundles`
       - **Variables Data:**
         - **Key:** `bundles`
         - **Value:**

           ```json
           {
             "bundle-1": {
               "title": "Bundle A",
               "products": ["Jacket", "Shirt"],
               "price": "$30"
             },
             "bundle-2": {
               "title": "Bundle B",
               "products": ["Pants", "Shoe"],
               "price": "$20"
             }
           }
           ```

## ReactJS Implementation Example

```javascript
import { useState, useEffect, useContext, createContext } from 'react';
import ConvertSDK from '@convertcom/js-sdk';

const UserContext = createContext();

const convertSDK = new ConvertSDK({ sdkKey: 'your_sdk_key_here' });

const ProductsComponent = () => {
  const { sdkContext } = useContext(UserContext);
  const [withHalloweenBundles, setWithHalloweenBundles] = useState(false);
  const [halloweenBundles, setHalloweenBundles] = useState([]);

  const decide = () => {
    const bucketedFeatureRollout = sdkContext.runExperience('halloween-bundles', {
      locationProperties: { location: 'homepage' }
    });
    if (bucketedFeatureRollout?.status === 'enabled') {
      setWithHalloweenBundles(true);
      if (bucketedFeatureRollout?.changes[0]?.data?.variables_data?.bundles) {
        setHalloweenBundles(Object.values(bucketedFeatureRollout.changes[0].data.variables_data.bundles));
      }
    }
  };

  useEffect(() => {
    if (sdkContext) {
      decide();
    }
  }, [sdkContext]);

  return (
    <div className="homepage">
      {withHalloweenBundles && (
        <div className="halloween-bundles">
          <h1>Halloween Bundles</h1>
          {halloweenBundles.map((bundle) => (
            <div className="bundle" key={bundle.title}>
              <div className="bundle-title">{bundle.title}</div>
              <div className="bundle-products">
                {bundle.products.map((product) => (
                  <div className="bundle-product" key={product}>{product}</div>
                ))}
              </div>
              <div className="bundle-price">{bundle.price}</div>
            </div>
          ))}
        </div>
      )}

      <div className="products recent-products">
        <div className="product">
          <div className="product-title">Shirt</div>
          <div className="product-price">$10</div>
        </div>

        <div className="product">
          <div className="product-title">Shoe</div>
          <div className="product-price">$50</div>
        </div>
      </div>
    </div>
  );
};

const App = () => {
  const userId = Date.now().toString(); // In reality, this should be the visitor ID, e.g., email, username, GUID, etc.
  const [sdkContext, setSdkContext] = useState();

  useEffect(() => {
    const initSdk = async () => {
      try {
        await convertSDK.onReady();
        const context = convertSDK.createContext(userId); // Convert SDK context needs to be created only once, hence the use of React Context below.

        // Set custom segments for the specific user
        context.setCustomSegments('halloween-segment', {
          ruleData: {
            halloween: true
          }
        });

        setSdkContext(context);
      } catch (error) {
        console.error('SDK Error:', error);
      }
    };
    initSdk();
  }, []);

  return (
    <UserContext.Provider value={{ sdkContext }}>
      <ProductsComponent />
    </UserContext.Provider>
  );
};

export default App;
```
