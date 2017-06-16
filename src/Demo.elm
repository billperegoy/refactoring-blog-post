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


transformOneField : FieldName -> Bool -> FieldSelection -> FieldSelection -> List Field -> List Field
transformOneField fieldName updateValue trueValue falseValue fields =
    fields
        |> List.map (\field -> conditionalFieldTransform fieldName updateValue trueValue falseValue field)


conditionalFieldTransform : FieldName -> Bool -> FieldSelection -> FieldSelection -> Field -> Field
conditionalFieldTransform fieldName selectStatus trueValue falseValue field =
    if field.fieldName == fieldName then
        setSelectionStatus fieldName selectStatus trueValue falseValue field
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
        SetSelected fieldName value ->
            { model
                | contactFields =
                    transformOneField fieldName value (Selected Optional) Unselected model.contactFields
            }
                ! []

        SetRequired fieldName value ->
            { model
                | contactFields =
                    transformOneField fieldName value (Selected Required) (Selected Optional) model.contactFields
            }
                ! []
