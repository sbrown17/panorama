-- Type signatures for both the editable and the validated version of a Person.

-- Key function signatures involved in translating an editable version of a Person to a validated Person. Do not
-- worry about implementing these.

  -- If there are any validations that cannot be represented in types then please do include a few code
  -- comments discussing those.

-- Briefly describe the Module organization for these types and what should be exposed and what should be
-- hidden. Be ready to discuss how this impacts total system maintainability.

module PersonValidation exposing (submitPerson)

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
  , sSN : String
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


submitPerson : ProvisionalPerson -> Person
-- 