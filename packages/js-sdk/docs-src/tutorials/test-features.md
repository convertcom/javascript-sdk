## Assumptions

You want to show one feature to 20% of your visitors on the `homepage` view of your e-commerce app.
Open your Fullstack project at the web interface and create a location for matching the target view, given the following rule:

1. Rule Type: `generic_text_key_value`
2. Match type: `matches`
3. Key: `location`
4. Value: `homepage`

Next, create a feature, given the following:

1. Name: `Special Deals`
2. Key: `special-deals`
3. Variables:
   1. Key: `products`
   2. Type: `json`

Then create one experience with one variation (_beside the original_) linked to the above location and feature, given the following:

Variation 1:

1. Traffic: `20%`
2. Key: `with-special-deals`
3. Changes:
   1. Type: `fullStackFeature`
   2. Feature: `Special Deals`
   3. Variables Data:
      1. Key: `products`
      2. Value:
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

## ReactJS Example

```javascript
import {useState, useEffect, useContext, createContext} from 'react';
import ConvertSDK from '@convertcom/js-sdk';

const UserContext = createContext();

const convertSDK = new ConvertSDK({sdkKey: 'xxx'});

const ProductsComponent = () => {
  const {sdkContext} = useContext(UserContext);
  const [withSpecialDeals, setWithSpecialDeals] = useState(false);
  const [specialDeals, setSpecialDeals] = useState([]);

  const decide = () => {
    const bucketedFeature = sdkContext.runFeature('special-deals', {
      locationProperties: {location: 'homepage'}
    });
    if (!!bucketedFeature && bucketedFeature?.status === 'enabled') {
      setWithSpecialDeals(true);

      if (bucketedFeature?.variables?.products) {
        setSpecialDeals(Object.values(bucketedFeature?.variables?.products));
      }
    }
  };

  useEffect(() => sdkContext && decide(), [sdkContext]);

  return (
    <div className="homapage">
      {withSpecialDeals && (
        <div className="special-deals">
          <h1>Special Deals</h1>
          {specialDeals.map((product) => (
            <div className="product">
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
