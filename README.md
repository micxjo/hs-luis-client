# hs-luis-client

An unofficial Haskell client for Microsoft's [LUIS](https://www.luis.ai) natural language processing API.

## Examples

```haskell
import Control.Lens
import NLP.LUIS

main = do
   let creds = Credentials "Your-App-Id" "Your-Subscription-Key"
   resp <- queryExc creds "show me news about greenland"

   resp ^? responseIntents . ix 0 . intentType   -- Just "FindNews"
   resp ^? responseEntities . ix 0 . entityText  -- Just "greenland"

```
