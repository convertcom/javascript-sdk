# Testing Feature Flags

## Assumptions

You want to show one feature to 20% of your visitors on the `homepage` view of your e-commerce app.

## Steps to Set Up

### Step 1: Create a Location

1. Open your Fullstack project at the web interface.
2. Create a location with the following details:
   - **Rule Type:** `generic_text_key_value`
   - **Match Type:** `matches`
   - **Key:** `location`
   - **Value:** `homepage`

### Step 2: Create a Feature

1. Create a feature with the following details:
   - **Name:** `Special Deals`
   - **Key:** `special-deals`
   - **Variables:**
     - **Key:** `products`
     - **Type:** `json`

### Step 3: Create an Experience

1. Create an experience with one variation (beside the original) linked to the above location and feature:
   - **Variation 1:**
     - **Traffic:** `20%`
     - **Key:** `with-special-deals`
     - **Changes:**
       - **Type:** `fullStackFeature`
       - **Feature:** `Special Deals`
       - **Variables Data:**
         - **Key:** `products`
         - **Value:**

           ```json
           {
             "jacket": {
               "title": "Jacket",
               "original_price": "$40",
               "price": "$25"
             },
             "pants": {
               "title": "Pants",
               "original_price": "$20",
               "price": "$10"
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
  const [withSpecialDeals, setWithSpecialDeals] = useState(false);
  const [specialDeals, setSpecialDeals] = useState([]);

  const decide = () => {
    const bucketedFeature = sdkContext.runFeature('special-deals', {
      locationProperties: { location: 'homepage' }
    });
    if (bucketedFeature?.status === 'enabled') {
      setWithSpecialDeals(true);
      if (bucketedFeature.variables?.products) {
        setSpecialDeals(Object.values(bucketedFeature.variables.products));
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
      {withSpecialDeals && (
        <div className="special-deals">
          <h1>Special Deals</h1>
          {specialDeals.map((product) => (
            <div className="product" key={product.title}>
              <div className="product-title">{product.title}</div>
              <div className="product-price">
                <strike>{product.original_price}</strike> {product.price}
              </div>
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
