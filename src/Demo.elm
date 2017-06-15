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


updateFieldRequirement : FieldName -> Bool -> Field -> Field
updateFieldRequirement fieldName selectStatus field =
    if field.fieldName == fieldName then
        if selectStatus then
            { field | selectionStatus = Selected Required }
        else
            { field | selectionStatus = Selected Optional }
    else
        field


updateSelectStatus : FieldName -> Bool -> Field -> Field
updateSelectStatus fieldName selectStatus field =
    if field.fieldName == fieldName then
        if selectStatus then
            { field | selectionStatus = Selected Optional }
        else
            { field | selectionStatus = Unselected }
    else
        field


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectField fieldName selectStatus ->
            let
                contactFields =
                    model.contactFields
                        |> List.map (\field -> updateSelectStatus fieldName selectStatus field)
            in
                { model | contactFields = contactFields } ! []

        SetRequired fieldName requiredValue ->
            let
                contactFields =
                    model.contactFields
                        |> List.map (\field -> updateFieldRequirement fieldName requiredValue field)
            in
                { model | contactFields = contactFields } ! []
