# Testing Variations

## Assumptions

You want to show 2 variations only to 50% of your visitors on the `products` view of your e-commerce app.

## Steps to Set Up

### Step 1: Create a Location

1. Open your Fullstack project at the web interface.
2. Create a location with the following details:
   - **Rule Type:** `generic_text_key_value`
   - **Match Type:** `matches`
   - **Key:** `location`
   - **Value:** `products`

### Step 2: Create Experiences

Create two experiences, each with one variation (beside the original) linked to the above location:

#### Variation 1

1. **Traffic:** `50%`
2. **Key:** `with-stock-amount`

#### Variation 2

1. **Traffic:** `50%`
2. **Key:** `with-headline`

## ReactJS Implementation Example

```javascript
import { useState, useEffect, useContext, createContext } from 'react';
import ConvertSDK from '@convertcom/js-sdk';

const UserContext = createContext();

const convertSDK = new ConvertSDK({ sdkKey: 'your_sdk_key_here' });

const ProductsComponent = () => {
  const { sdkContext } = useContext(UserContext);
  const [withStock, setWithStock] = useState(false);
  const [withHeadline, setWithHeadline] = useState(false);

  const decide = () => {
    const bucketedVariations = sdkContext.runExperiences({
      locationProperties: { location: 'products' }
    });
    bucketedVariations.forEach((variation) => {
      if (variation.experienceKey === 'with-stock-amount') {
        setWithStock(true);
      }
      if (variation.experienceKey === 'with-headline') {
        setWithHeadline(true);
      }
    });
  };

  useEffect(() => {
    if (sdkContext) {
      decide();
    }
  }, [sdkContext]);

  return (
    <div className="products">
      <div className="product">
        <div className="product-title">Shirt</div>
        {withHeadline && <div className="product-headline">Rare item</div>}
        <div className="product-price">$10</div>
        {withStock && <div className="product-stock">in stock</div>}
      </div>

      <div className="product">
        <div className="product-title">Shoe</div>
        {withHeadline && <div className="product-headline">Natural leather</div>}
        <div className="product-price">$50</div>
        {withStock && <div className="product-stock">out of stock</div>}
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
    <UserContext.Provider value={{ sdkContext }}>
      <ProductsComponent />
    </UserContext.Provider>
  );
};

export default App;
```
