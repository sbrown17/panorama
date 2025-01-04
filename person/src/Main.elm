--Module organization:
-- The only thing I'd really want to expose is submitPerson, everything else should be handled by this module and wont be needed by the user of the api
-- also organized it to be more relaxed on typing up front and give more strict typing after verification so it can be trusted more thoroughly when in use

module PersonValidation exposing (submitPerson)

type alias ProvisionalPerson =
    { firstName : Maybe String
    , lastName : Maybe String
    , socialSecurity : Maybe String
    , married : Maybe Bool
    , phoneNumber : Maybe String
    }

type alias ValidatedPerson =
    { firstName : FirstName
    , lastName : LastName
    , socialSecurity : SocialSecurity
    , married : MarriageStatus
    , phoneNumber : PhoneNumber
    }

type FirstName
    = FirstName String
type LastName
    = LastName String

type MarriageStatus
    = Married
    | Unmarried

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

type alias ValidationResult error success
    = ValidationFailure (List error)
    | ValidationSuccess success

type alias PersonResult = 
    ValidationResult String ValidatedPerson

type alias PhoneNumberResult =
    ValidationResult String String

submitPerson : ProvisionalPerson -> PersonResult
-- verifyFirstName/LastName/SocialSecurity/PhoneNumber/MarriageStatus

verifyFirstName : String -> FirstName

verifyLastName : String -> LastName

verifySocialSecurity : String -> SocialSecurity
-- this should be encrypted somehow to preserve user privacy

verifyPhoneNumber : String -> PhoneNumberResult
-- should also probably be encrypted

verifyMarriageStatus : Bool -> MarriageStatus