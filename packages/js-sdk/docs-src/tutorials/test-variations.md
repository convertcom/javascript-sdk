## Assumptions

You want to show 2 variations only to 50% of your visitors on the `products` view of your e-commerce app.
Open your Fullstack project at the web interface and create a location for matching the target view, given the following rule:

1. Rule Type: `generic_text_key_value`
2. Match type: `matches`
3. Key: `location`
4. Value: `products`

Then create 2 experiences each with one variation (_beside the original_) linked to the above location, given the following unique identifiers:

Variation 1:

1. Traffic: `50%`
2. Key: `with-stock-amount`

Variation 2:

1. Traffic: `50%`
2. Key: `with-headline`

## ReactJS Example

```javascript
import {useState, useEffect, useContext, createContext} from 'react';
import ConvertSDK from '@convertcom/js-sdk';

const UserContext = createContext();

const convertSDK = new ConvertSDK({sdkKey: 'xxx'});

const ProductsComponent = () => {
  const {sdkContext} = useContext(UserContext);
  const [withStock, setWithStock] = useState(false);
  const [withHeadline, setWithHeadline] = useState(false);

  const decide = () => {
    const bucketedVariations = sdkContext.runExperiences({
      locationProperties: {location: 'products'}
    });
    bucketedVariations.forEach((e) => {
      if (e.experienceKey === 'with-stock-amount') {
        setWithStock(true);
      }
      if (e.experienceKey === 'with-headline') {
        setWithHeadline(true);
      }
    });
  };

  useEffect(() => sdkContext && decide(), [sdkContext]);

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
        {withHeadline && (
          <div className="product-headline">Natural leather</div>
        )}
        <div className="product-price">$50</div>
        {withStock && <div className="product-stock">out of stock</div>}
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
