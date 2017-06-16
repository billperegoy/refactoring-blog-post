module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Demo exposing (..)


all : Test
all =
    describe "A Test Suite"
        [ test "can select a field in a single element list" <|
            let
                model =
                    { contactFields = [ Field Email Unselected ] }

                updatedModel =
                    update (SetSelected Email True) model

                expectedResult =
                    { contactFields = [ Field Email (Selected Optional) ] } ! []
            in
                \() ->
                    Expect.equal updatedModel expectedResult
        , test "can select a field in multi-element list" <|
            let
                model =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName Unselected
                        , Field LastName Unselected
                        ]
                    }

                updatedModel =
                    update (SetSelected FirstName True) model

                expectedResult =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Optional)
                        , Field LastName Unselected
                        ]
                    }
                        ! []
            in
                \() ->
                    Expect.equal updatedModel expectedResult
        , test "can unselect a field in multi-element list" <|
            let
                model =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Optional)
                        , Field LastName Unselected
                        ]
                    }

                updatedModel =
                    update (SetSelected FirstName False) model

                expectedResult =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName Unselected
                        , Field LastName Unselected
                        ]
                    }
                        ! []
            in
                \() ->
                    Expect.equal updatedModel expectedResult
        , test "can unselecting  an unslected field causes no change" <|
            let
                model =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName Unselected
                        , Field LastName Unselected
                        ]
                    }

                updatedModel =
                    update (SetSelected FirstName False) model

                expectedResult =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName Unselected
                        , Field LastName Unselected
                        ]
                    }
                        ! []
            in
                \() ->
                    Expect.equal updatedModel expectedResult
        , test "selecting an selected field causes no change" <|
            let
                model =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Optional)
                        , Field LastName Unselected
                        ]
                    }

                updatedModel =
                    update (SetSelected FirstName True) model

                expectedResult =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Optional)
                        , Field LastName Unselected
                        ]
                    }
                        ! []
            in
                \() ->
                    Expect.equal updatedModel expectedResult
        , test "can make a field required in multi-element list" <|
            let
                model =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Optional)
                        , Field LastName Unselected
                        ]
                    }

                updatedModel =
                    update (SetRequired FirstName True) model

                expectedResult =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Required)
                        , Field LastName Unselected
                        ]
                    }
                        ! []
            in
                \() ->
                    Expect.equal updatedModel expectedResult
        , test "can make a field optional in multi-element list" <|
            let
                model =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Required)
                        , Field LastName Unselected
                        ]
                    }

                updatedModel =
                    update (SetRequired FirstName False) model

                expectedResult =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Optional)
                        , Field LastName Unselected
                        ]
                    }
                        ! []
            in
                \() ->
                    Expect.equal updatedModel expectedResult
        , test "making an optional field optional causes no change" <|
            let
                model =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Optional)
                        , Field LastName Unselected
                        ]
                    }

                updatedModel =
                    update (SetRequired FirstName False) model

                expectedResult =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Optional)
                        , Field LastName Unselected
                        ]
                    }
                        ! []
            in
                \() ->
                    Expect.equal updatedModel expectedResult
        , test "making a required field required causes no change" <|
            let
                model =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Required)
                        , Field LastName Unselected
                        ]
                    }

                updatedModel =
                    update (SetRequired FirstName True) model

                expectedResult =
                    { contactFields =
                        [ Field Email Unselected
                        , Field FirstName (Selected Required)
                        , Field LastName Unselected
                        ]
                    }
                        ! []
            in
                \() ->
                    Expect.equal updatedModel expectedResult
        ]
