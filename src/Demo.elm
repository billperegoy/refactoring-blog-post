module Demo exposing (..)


type alias Field =
    { fieldName : FieldName
    , selectionStatus : FieldSelection
    }


type FieldName
    = Email
    | FirstName
    | LastName
    | PhoneNumber
    | Country
    | Street
    | City
    | State
    | PostalCode
    | Company
    | JobTitle
    | Birthday
    | Anniversary


type FieldSelection
    = Immutable
    | Unselected
    | Selected FieldRequirement


type FieldRequirement
    = Required
    | Optional


type alias Model =
    { contactFields : List Field
    }


type Msg
    = SetSelected FieldName Bool
    | SetRequired FieldName Bool


updateMatchingField : FieldName -> FieldSelection -> List Field -> List Field
updateMatchingField fieldName value fields =
    List.map (updateOnMatch fieldName value) fields


updateOnMatch : FieldName -> FieldSelection -> Field -> Field
updateOnMatch fieldName value field =
    if field.fieldName == fieldName then
        { field | selectionStatus = value }
    else
        field


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSelected fieldName True ->
            { model | contactFields = updateMatchingField fieldName (Selected Optional) model.contactFields } ! []

        SetSelected fieldName False ->
            { model | contactFields = updateMatchingField fieldName Unselected model.contactFields } ! []

        SetRequired fieldName True ->
            { model | contactFields = updateMatchingField fieldName (Selected Required) model.contactFields } ! []

        SetRequired fieldName False ->
            { model | contactFields = updateMatchingField fieldName (Selected Optional) model.contactFields } ! []
