{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedString #-}

module Contracts.TokenRecycler where

import Data.Map qualified as Map
import PlutusTX
import PlutusTX.Prelude
import Ledger
import Ledger.Typed.Script



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

sample :: SenderPKH
sample = "7fee02606ae2e089831ec9c574d92b55d1ccec80a5a73c1230fde947"

mkRecycleDatum :: SenderPKH -> TokenValue -> CurrencySymbol -> RecycleDatum
mkRecycleDatum senderPKH tokenValue cr =
  RecycleDatum
    { recieverPKH = senderPKH
    , tokenValue = tokenValue
    , policyId = cr
    }
          
compiledValidator :: SenderPKH -> Validator
compiledValidator pkh = mkValidatorScript $
  $$(PlutusTx.compile [|| \senderPKH' -> untypedLambda senderPKH' ||])
  `PlutusTx.applyCode` PlutusTx.liftCode pkh

writeValidatorToFile :: IO ()
writeValidatorToFile = do
  let validatorScript = compiledValidator sample
  writeFile "recycle.plutus" (show validatorScript)