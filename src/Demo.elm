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
    = SelectField FieldName Bool
    | SetRequired FieldName Bool


transformOneField : (FieldName -> Bool -> Field -> Field) -> FieldName -> Bool -> List Field -> List Field
transformOneField updateFunction fieldName updateValue fields =
    fields
        |> List.map (\field -> updateFunction fieldName updateValue field)


updateFieldRequirement : FieldName -> Bool -> Field -> Field
updateFieldRequirement fieldName selectStatus field =
    if field.fieldName == fieldName then
        setSelectionStatus fieldName selectStatus (Selected Required) (Selected Optional) field
    else
        field


updateSelectStatus : FieldName -> Bool -> Field -> Field
updateSelectStatus fieldName selectStatus field =
    if field.fieldName == fieldName then
        setSelectionStatus fieldName selectStatus (Selected Optional) Unselected field
    else
        field


setSelectionStatus : FieldName -> Bool -> FieldSelection -> FieldSelection -> Field -> Field
setSelectionStatus fieldName selectStatus trueValue falseValue field =
    if selectStatus then
        { field | selectionStatus = trueValue }
    else
        { field | selectionStatus = falseValue }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectField fieldName selectStatus ->
            { model
                | contactFields =
                    transformOneField updateSelectStatus fieldName selectStatus model.contactFields
            }
                ! []

        SetRequired fieldName requiredValue ->
            { model
                | contactFields =
                    transformOneField updateFieldRequirement fieldName requiredValue model.contactFields
            }
                ! []
