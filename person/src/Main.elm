--Module organization:
-- The only thing I'd really want to expose is editPerson and submitPerson, everything else should be handled by this module and wont be needed by the user of the api

module PersonValidation exposing (editPerson, submitPerson)

type alias ProvisionalPerson =
  { firstName : Maybe String
  , lastName : Maybe String
  , socialSecurity : Maybe SocialSecurity
  , married : Maybe Bool
  , phoneNumber : Maybe PhoneNumber
  }

type alias Person =
  { firstName : String
  , lastName : String
  , socialSecurity : String
  , married : Bool
  , phoneNumber : String
  }

type alias SocialSecurity =
  { areaNumber : Int
  , groupNumber : Int
  , serialNumber : Int
  }

type alias PhoneNumber =
  { areaCode : Int
  , telephonePrefix : Int
  , lineNumber : Int
  }

editPerson : ProvisionalPerson -> ProvisionalPerson
-- editPerson should check for a unique id (uid) and if the client is vetted, they should be allowed to make changes to the ProvisionalPerson
-- just in case they wish to come back later

submitPerson : ProvisionalPerson -> Person -> (Bool, String)
-- should return a success/failure and a message of what went right or wrong  (eg. "Information Validated and Submitted Successfully" or "Failure to validate."/"Failure to submit, please try again.")

verifySocialSecurity : SocialSecurity -> String
-- this should be encrypted somehow to preserve user privacy

verifyPhoneNumber : PhoneNumber -> String
-- should also probably be encrypted