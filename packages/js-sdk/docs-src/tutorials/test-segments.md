## Assumptions

You want to show one feature rollout to a sprcific segment of your visitors on the `homepage` view of your e-commerce app.
Open your Fullstack project at the web interface and create a location for matching the target view, given the following rule:

1. Rule Type: `generic_text_key_value`
2. Match type: `matches`
3. Key: `location`
4. Value: `homepage`

Next, create an audience (_represents your segmentation in this case_), given the following:

1. Name: `Halloween Segment`
2. Type: `segmentation`
3. Key: `halloween-segment`
4. Rules:
   1. Rule Type: `generic_bool_key_value`
   2. Match type: `equals`
   3. Key: `halloween`
   4. Value: `true`

Next, create a feature, given the following:

1. Name: `Halloween Bundles`
2. Key: `halloween-bundles`
3. Variables:
   1. Key: `bundles`
   2. Type: `json`

Then create one experience with one variation (_**without** the original_) linked to the above location, segmentation, and feature, given the following:

Variation 1:

1. Traffic: `100%`
2. Key: `with-halloween-bundles`
3. Changes:
   1. Type: `fullStackFeature`
   2. Feature: `Halloween Bundles`
   3. Variables Data:
      1. Key: `bundles`
      2. Value:
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

## ReactJS Example

```javascript
import {useState, useEffect, useContext, createContext} from 'react';
import ConvertSDK from '@convertcom/js-sdk';

const UserContext = createContext();

const convertSDK = new ConvertSDK({sdkKey: 'xxx'});

const ProductsComponent = () => {
  const {sdkContext} = useContext(UserContext);
  const [withHalloweenBundles, setWithHalloweenBundles] = useState(false);
  const [halloweenBundles, setHalloweenBundles] = useState([]);

  const decide = () => {
    const bucketedFeatureRollout = sdkContext.runExperience(
      'halloween-bundles',
      {
        locationProperties: {location: 'homepage'}
      }
    );
    if (
      !!bucketedFeatureRollout &&
      bucketedFeatureRollout?.status === 'enabled'
    ) {
      setWithHalloweenBundles(true);

      if (bucketedFeatureRollout?.changes[0]?.data?.variables_data?.bundles) {
        setHalloweenBundles(
          Object.values(
            bucketedFeatureRollout?.changes[0]?.data?.variables_data?.bundles
          )
        );
      }
    }
  };

  useEffect(() => sdkContext && decide(), [sdkContext]);

  return (
    <div className="homapage">
      {withHalloweenBundles && (
        <div className="halloween-bundles">
          <h1>Halloween Bundles</h1>
          {halloweenBundles.map((bundle) => (
            <div className="bundle">
              <div className="bundle-title">{bundle.title}</div>
              <div className="bundle-products">
                {bundle.products.map((product) => (
                  <div className="bundle-product">{product}</div>
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

export default function App() {
  const userId = Date.now().toString(); // in reality, this should be the visitor ID. Fur example: email, username, GUID, .. etc.

  const [sdkContext, setSdkContext] = useState();

  useEffect(() => {
    async function initSdk() {
      try {
        await convertSDK.onReady();
        const context = convertSDK.createContext(userId); // Convert SDK context needs to be created only once, hence the use of React Context below.

        // shared across all visitors you wish to fall under the same segment
        // in reality, you need to perform this only on specific users
        context.setCustomSegments('halloween-segment', {
          ruleData: {
            halloween: true
          }
        });

        setSdkContext(context);
      } catch (error) {
        console.error('SDK Error:', error);
      }
    }
    initSdk();
  }, []);

  return (
    <UserContext.Provider value={{sdkContext}}>
      <ProductsComponent />
    </UserContext.Provider>
  );
}
```
