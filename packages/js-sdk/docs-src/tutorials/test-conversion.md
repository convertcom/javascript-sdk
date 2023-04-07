## Assumptions

You want to track 2 features shown to 50% of your visitors on the `products` view of your e-commerce app.
Open your Fullstack project at the web interface and create a location for matching the target view, given the following rule:

1. Rule Type: `generic_text_key_value`
2. Match type: `matches`
3. Key: `location`
4. Value: `products`

Next, create a goal, given the following:

1. Name: `Buying Goal`
2. Type: `revenue`
3. Key: `buying-goal`
4. Triggering Rule:
   1. Rule Type: `generic_text_key_value`
   2. Match type: `matches`
   3. Key: `action`
   4. Value: `buy`
5. Settings:
   1. Triggering Type: `manual`

Next, create 2 features, given the following:

Feature 1:

1. Name: `Buying Options`
2. Key: `buying-options`
3. Variables:
   1. Key: `options`
   2. Type: `json`

Feature 2:

1. Name: `Buy with Discount`
2. Key: `buy-discount`
3. Variables:
   1. Key: `discount`
   2. Type: `integer`

Then create one experience with one variation (_beside the original_) linked to the above location, goal, and features, given the following unique identifiers:

Variation 1:

1. Traffic: `50%`
2. Key: `with-buying-options`

## ReactJS Example

```javascript
import {useState, useEffect, useContext, createContext} from 'react';
import ConvertSDK from '@convertcom/js-sdk';

const UserContext = createContext();

const convertSDK = new ConvertSDK({sdkKey: 'xxx'});

const ProductsComponent = () => {
  const {sdkContext} = useContext(UserContext);
  const [withBuyingOptions, setWithBuyingOptions] = useState(false);
  const [buyingOptions, setBuyingOptions] = useState([]);
  const [withDiscount, setWithDiscount] = useState(0);

  const decide = () => {
    const bucketedFeatures = sdkContext.runFeatures({
      locationProperties: {location: 'products'}
    });
    bucketedFeatures.forEach((e) => {
      if (e.key === 'buying-options' && e?.variables?.options) {
        setWithBuyingOptions(true);
        setBuyingOptions(Object.values(e?.variables?.options));
      }
      if (e.key === 'buy-discount' && e?.variables?.discount) {
        setWithDiscount(e?.variables?.discount);
      }
    });
  };

  useEffect(() => sdkContext && decide(), [sdkContext]);

  const track = (price, quantity = 1) => {
    sdkContext.trackConversion('buying-goal', {
      ruleData: {
        action: 'buy'
      },
      conversionData: [
        {
          amount: price,
          productsCount: quantity
        }
      ]
    });
  };

  const BuyOption = (option, price, discount = 0) => (
    <button
      className="product-buy"
      onClick={(e) => {
        e.preventDefault();
        const price = 10;
        if (discount) {
          track((price * discount) / 100, 1);
        } else {
          track(price, 1);
        }
      }}
    >
      Buy via {option}
    </button>
  );

  return (
    <div className="products">
      <div className="product">
        <div className="product-title">Shirt</div>
        <div className="product-price">$10</div>
        <BuyOption option="Credit Card" price={10} discount={withDiscount} />
        {withBuyingOptions && (
          <div className="buying-options">
            <h1>Buying Options</h1>
            {buyingOptions.map((option) => (
              <BuyOption option={option} price={10} discount={withDiscount} />
            ))}
          </div>
        )}
      </div>

      <div className="product">
        <div className="product-title">Shoe</div>
        <div className="product-price">$50</div>
        <BuyOption option="Credit Card" price={50} discount={withDiscount} />
        {withBuyingOptions && (
          <div className="buying-options">
            <h1>Buying Options</h1>
            {buyingOptions.map((option) => (
              <BuyOption option={option} price={50} discount={withDiscount} />
            ))}
          </div>
        )}
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
