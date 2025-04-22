{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Contracts.TokenRecycler where

import Plutus.V2.Ledger.Api
import PlutusTx
import PlutusTx.Prelude
import Prelude (IO, Show (..), print, String)
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Contexts as V2
import qualified Plutus.V2.Ledger.Tx as V2
import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.Script.Utils.V2.Scripts (Validator)
import qualified Plutus.V2.Ledger.Value as Value
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Plutus.Script.Utils.V2.Scripts (writeValidatorToFile)



type SenderPKH = PubKeyHash

type HostPKH = PubKeyHash

type TokenValue = Value


  -- Define RecycleDatum data type
data RecycleDatum = RecycleDatum 
  { recieverPKH :: SenderPKH
  , tokenValue :: TokenValue
  , policyId :: CurrencySymbol
  }
PlutusTx.unstableMakeIsData ''RecycleDatum


-- | Helper function to check if the given value has at least one token of the given currency symbol and token name.
hasToken :: CurrencySymbol -> TokenName -> Value -> Bool
hasToken cs tn val = Value.valueOf val cs tn >= 1
{-# INLINEABLE hasToken #-}

setReceiverAddress :: ScriptContext -> Maybe PubKeyHash
setReceiverAddress (ScriptContext txInfo _) =
      case txInfoSignatories txInfo of
        [] -> Nothing
        (pkh:_) -> Just pkh
{-#INLINABLE setReceiverAddress #-}


    -- Function to calculate the reward based on the output values in txInfo
rewardX :: ScriptContext -> Value
rewardX (ScriptContext txInfo _) =
  case txInfoOutputs txInfo of
    [] -> mempty
    (rewardTxOut:_) -> scaleValueByThree $ txOutValue rewardTxOut
  where
    scaleValueByThree :: Value -> Value
    scaleValueByThree value = scale 3 value
{-#INLINABLE rewardX #-}


validatorR :: SenderPKH -> Datum -> Redeemer -> ScriptContext -> Bool
validatorR senderPKH datum _ ctx@(ScriptContext txInfo _) =
  case setReceiverAddress ctx of
    Nothing -> traceError "No signer"
    Just receiverPKH ->
      let
          rewardValue = rewardX ctx
          outVal = case txInfoOutputs (scriptContextTxInfo ctx) of
                      [] -> mempty
                      (out:_) -> txOutValue out
        in
          if receiverPKH == senderPKH &&
             hasToken (CurrencySymbol "") (TokenName "") outVal &&
             outVal == rewardValue
          then ()
          else traceError "Validation failed"
{-#INLINABLE validatorR #-}

untypedLambda :: SenderPKH -> UntypedValidator  
untypedLambda = mkUntypedValidator . validatorR
{-#INLINABLE untypedLambda #-}

type TokenRecycle = ValidatorContract "cyc"

mkRecycleDatum :: SenderPKH -> TokenValue -> CurrencySymbol -> RecycleDatum
mkRecycleDatum senderPKH tokenValue cr =
  RecycleDatum
    { recieverPKH = senderPKH
    , tokenValue = tokenValue
    , policyId = cr
    }
          
compiledValidator :: SenderPKH -> Validator
compiledValidator pkh = mkValidatorScript $
  $$(PlutusTx.compile [|| \_ -> untypedLambda () ||])

writeValidatorToFile :: IO ()
writeValidatorToFile = do
  let validatorScript = compiledValidator 
  writeFile "recycle.plutus" (show validatorScript)