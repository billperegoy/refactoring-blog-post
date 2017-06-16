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


mapOnFieldMatch : FieldName -> FieldSelection -> List Field -> List Field
mapOnFieldMatch fieldName value fields =
    fields
        |> List.map (\field -> conditionalFieldTransform fieldName value field)


conditionalFieldTransform : FieldName -> FieldSelection -> Field -> Field
conditionalFieldTransform fieldName value field =
    if field.fieldName == fieldName then
        { field | selectionStatus = value }
    else
        field


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSelected fieldName True ->
            { model | contactFields = mapOnFieldMatch fieldName (Selected Optional) model.contactFields } ! []

        SetSelected fieldName False ->
            { model | contactFields = mapOnFieldMatch fieldName Unselected model.contactFields } ! []

        SetRequired fieldName True ->
            { model | contactFields = mapOnFieldMatch fieldName (Selected Required) model.contactFields } ! []

        SetRequired fieldName False ->
            { model | contactFields = mapOnFieldMatch fieldName (Selected Optional) model.contactFields } ! []
