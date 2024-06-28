# Testing Conversion Events

## Assumptions

You want to track 2 features shown to 50% of your visitors on the `products` view of your e-commerce app.

## Steps to Set Up

### Step 1: Create a Location

1. Open your Fullstack project at the web interface.
2. Create a location with the following details:
   - **Rule Type:** `generic_text_key_value`
   - **Match Type:** `matches`
   - **Key:** `location`
   - **Value:** `products`

### Step 2: Create a Goal

1. Create a goal with the following details:
   - **Name:** `Buying Goal`
   - **Type:** `revenue`
   - **Key:** `buying-goal`
   - **Triggering Rule:**
     - **Rule Type:** `generic_text_key_value`
     - **Match Type:** `matches`
     - **Key:** `action`
     - **Value:** `buy`
   - **Settings:**
     - **Triggering Type:** `manual`

### Step 3: Create Features

1. Create Feature 1:
   - **Name:** `Buying Options`
   - **Key:** `buying-options`
   - **Variables:**
     - **Key:** `options`
     - **Type:** `json`

2. Create Feature 2:
   - **Name:** `Buy with Discount`
   - **Key:** `buy-discount`
   - **Variables:**
     - **Key:** `discount`
     - **Type:** `integer`

### Step 4: Create an Experience

1. Create an experience with one variation (beside the original) linked to the above location, goal, and features:
   - **Variation 1:**
     - **Traffic:** `50%`
     - **Key:** `with-buying-options`

## ReactJS Implementation Example

```javascript
import {useState, useEffect, useContext, createContext} from 'react';
import ConvertSDK from '@convertcom/js-sdk';

const UserContext = createContext();

const convertSDK = new ConvertSDK({sdkKey: 'your_sdk_key_here'});

const ProductsComponent = () => {
  const {sdkContext} = useContext(UserContext);
  const [withBuyingOptions, setWithBuyingOptions] = useState(false);
  const [buyingOptions, setBuyingOptions] = useState([]);
  const [withDiscount, setWithDiscount] = useState(0);

  const decide = () => {
    const bucketedFeatures = sdkContext.runFeatures({
      locationProperties: {location: 'products'}
    });
    bucketedFeatures.forEach((feature) => {
      if (feature.key === 'buying-options' && feature.variables?.options) {
        setWithBuyingOptions(true);
        setBuyingOptions(Object.values(feature.variables.options));
      }
      if (feature.key === 'buy-discount' && feature.variables?.discount) {
        setWithDiscount(feature.variables.discount);
      }
    });
  };

  useEffect(() => {
    if (sdkContext) {
      decide();
    }
  }, [sdkContext]);

  const track = (price, quantity, id) => {
    sdkContext.trackConversion('buying-goal', {
      ruleData: {
        action: 'buy'
      },
      conversionData: [
        {
          key: 'amount',
          value: price
        },
        {
          key: 'productsCount',
          value: quantity
        },
        {
          key: 'transactionId',
          value: id
        }
      ]
    });
  };

  const BuyOption = ({option, price, discount = 0}) => (
    <button
      className="product-buy"
      onClick={(e) => {
        e.preventDefault();
        const price = 10,
          id = 'transaction-unique-id';
        if (discount) {
          track((price * discount) / 100, 1, id);
        } else {
          track(price, 1, id);
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
              <BuyOption key={option} option={option} price={10} discount={withDiscount} />
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
              <BuyOption key={option} option={option} price={50} discount={withDiscount} />
            ))}
          </div>
        )}
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
        setSdkContext(context);
      } catch (error) {
        console.error('SDK Error:', error);
      }
    };
    initSdk();
  }, []);

  return (
    <UserContext.Provider value={{sdkContext}}>
      <ProductsComponent />
    </UserContext.Provider>
  );
};

export default App;
```
