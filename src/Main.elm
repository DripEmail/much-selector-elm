module Main exposing (..)

import Browser
import Css exposing (block, display, fontSize, hidden, inline, lineHeight, none, px, top, visibility, visible, width)
import Html.Styled
    exposing
        ( Html
        , div
        , fromUnstyled
        , input
        , node
        , span
        , text
        , toUnstyled
        )
import Html.Styled.Attributes
    exposing
        ( class
        , classList
        , css
        , disabled
        , id
        , name
        , tabindex
        , type_
        , value
        )
import Html.Styled.Events exposing (onBlur, onClick, onFocus, onInput, onMouseDown, onMouseEnter, onMouseLeave)
import Json.Decode
import Keyboard exposing (Key(..))
import Keyboard.Events as Keyboard
import List.Extra exposing (mapAccuml)
import Option
    exposing
        ( Option(..)
        , OptionDisplay(..)
        , OptionGroup
        , OptionLabel(..)
        , OptionValue
        , highlightOptionInListByValue
        , removeHighlightOptionInList
        , selectHighlightedOption
        , selectOptionInListByOptionValue
        , selectOptionsInOptionsListByString
        , selectSingleOptionInList
        , selectedOptionsToTuple
        , updateOrAddCustomOption
        )
import OptionPresentor exposing (OptionPresenter)
import OptionSearcher exposing (bestMatch)
import Ports
    exposing
        ( addOptionsReceiver
        , blurInput
        , deselectOptionReceiver
        , disableChangedReceiver
        , errorMessage
        , focusInput
        , loadingChangedReceiver
        , maxDropdownItemsChangedReceiver
        , optionsChangedReceiver
        , placeholderChangedReceiver
        , removeOptionsReceiver
        , selectOptionReceiver
        , valueCasingDimensionsChangedReceiver
        , valueChanged
        , valueChangedReceiver
        , valuesDecoder
        )
import SelectionMode exposing (SelectionMode(..))


type Msg
    = NoOp
    | BringInputInFocus
    | BringInputOutOfFocus
    | InputBlur
    | InputFocus
    | DropdownMouseOverOption OptionValue
    | DropdownMouseOutOption OptionValue
    | DropdownMouseClickOption OptionValue
    | SearchInputOnInput String
    | ValueChanged Json.Decode.Value
    | OptionsChanged Json.Decode.Value
    | AddOptions Json.Decode.Value
    | RemoveOptions Json.Decode.Value
    | SelectOption Json.Decode.Value
    | DeselectOption Json.Decode.Value
    | PlaceholderAttributeChanged String
    | LoadingAttributeChanged Bool
    | MaxDropdownItemsChanged Int
    | DisabledAttributeChanged Bool
    | SelectHighlightedOption
    | DeleteInputForSingleSelect
    | EscapeKeyInInputFilter
    | MoveHighlightedOptionUp
    | MoveHighlightedOptionDown
    | ValueCasingWidthUpdate { width : Float, height : Float }
    | ClearAllSelectedOptions
      -- SelectedValueHighlight?!? WTF? Yes, this is because in multi selected
      --  mode a user can 'highlight' any number of the selected values by clicking
      --  on them and then delete them (with the delete key).
      --  If you can think of a better name we're all ears.
    | ToggleSelectedValueHighlight OptionValue
    | DeleteSelectedAndHighlightedValues


type alias Model =
    { initialValue : List String
    , placeholder : String
    , size : String
    , selectionMode : SelectionMode
    , options : List Option
    , showDropdown : Bool
    , searchString : String
    , rightSlot : RightSlot
    , maxDropdownItems : Int
    , disabled : Bool
    , focused : Bool
    , valueCasingWidth : Float
    , valueCasingHeight : Float
    , custom : Bool
    }


type RightSlot
    = ShowNothing
    | ShowLoadingIndicator
    | ShowDropdownIndicator
    | ShowClearButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        BringInputInFocus ->
            if model.focused then
                ( model, Cmd.none )

            else
                ( { model | focused = True }, focusInput () )

        BringInputOutOfFocus ->
            if model.focused then
                ( { model | focused = False }, blurInput () )

            else
                ( model, Cmd.none )

        InputBlur ->
            ( { model | showDropdown = False, focused = False }, Cmd.none )

        InputFocus ->
            ( { model | showDropdown = True }, Cmd.none )

        DropdownMouseOverOption optionValue ->
            ( { model | options = highlightOptionInListByValue optionValue model.options }, Cmd.none )

        DropdownMouseOutOption optionValue ->
            ( { model | options = removeHighlightOptionInList optionValue model.options }, Cmd.none )

        DropdownMouseClickOption optionValue ->
            let
                options =
                    case model.selectionMode of
                        MultiSelect ->
                            selectOptionInListByOptionValue optionValue model.options

                        SingleSelect ->
                            selectSingleOptionInList optionValue model.options
            in
            ( { model | options = options }, Cmd.batch [ valueChanged (selectedOptionsToTuple options), blurInput () ] )

        SearchInputOnInput string ->
            let
                options =
                    if showAddOption model.custom string then
                        updateOrAddCustomOption string model.options

                    else
                        model.options
            in
            case bestMatch string model.options of
                Just (Option _ _ value _ _) ->
                    ( { model | searchString = string, options = highlightOptionInListByValue value options }, Cmd.none )

                Just (CustomOption _ _ value) ->
                    ( { model | searchString = string, options = highlightOptionInListByValue value options }, Cmd.none )

                Just (EmptyOption _ _) ->
                    ( { model | searchString = string, options = options }, Cmd.none )

                Nothing ->
                    ( { model | searchString = string, options = options }, Cmd.none )

        ValueChanged valuesJson ->
            let
                valuesResult =
                    Json.Decode.decodeValue valuesDecoder valuesJson
            in
            case valuesResult of
                Ok values ->
                    ( { model
                        | options = selectOptionsInOptionsListByString values model.options
                        , rightSlot =
                            updateRightSlot
                                model.rightSlot
                                model.selectionMode
                                (Option.hasSelectedOption model.options)
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( model, errorMessage (Json.Decode.errorToString error) )

        OptionsChanged optionsJson ->
            case Json.Decode.decodeValue Option.optionsDecoder optionsJson of
                Ok newOptions ->
                    let
                        newOptionWithOldSelectedOption =
                            case model.selectionMode of
                                SingleSelect ->
                                    Option.mergeTwoListsOfOptionsPreservingSelectedOptions model.options newOptions

                                MultiSelect ->
                                    -- Also filter out any empty options.
                                    newOptions
                                        |> List.filter (not << Option.isEmptyOption)
                                        |> Option.mergeTwoListsOfOptionsPreservingSelectedOptions model.options
                    in
                    ( { model | options = newOptionWithOldSelectedOption }, Cmd.none )

                Err error ->
                    ( model, errorMessage (Json.Decode.errorToString error) )

        AddOptions optionsJson ->
            case Json.Decode.decodeValue Option.optionsDecoder optionsJson of
                Ok newOptions ->
                    ( { model | options = Option.addAdditionalOptionsToOptionList model.options newOptions }, Cmd.none )

                Err error ->
                    ( model, errorMessage (Json.Decode.errorToString error) )

        RemoveOptions optionsJson ->
            case Json.Decode.decodeValue Option.optionsDecoder optionsJson of
                Ok optionsToRemove ->
                    ( { model | options = Option.removeOptionsFromOptionList model.options optionsToRemove }, Cmd.none )

                Err error ->
                    ( model, errorMessage (Json.Decode.errorToString error) )

        SelectOption optionJson ->
            case Json.Decode.decodeValue Option.decoder optionJson of
                Ok option ->
                    let
                        optionValue =
                            Option.getOptionValue option

                        options =
                            case model.selectionMode of
                                MultiSelect ->
                                    selectOptionInListByOptionValue optionValue model.options

                                SingleSelect ->
                                    selectSingleOptionInList optionValue model.options
                    in
                    ( { model | options = options }, Cmd.batch [ valueChanged (selectedOptionsToTuple options) ] )

                Err error ->
                    ( model, errorMessage (Json.Decode.errorToString error) )

        DeselectOption optionJson ->
            case Json.Decode.decodeValue Option.decoder optionJson of
                Ok option ->
                    let
                        optionValue =
                            Option.getOptionValue option

                        options =
                            Option.deselectOptionInListByOptionValue optionValue model.options
                    in
                    ( { model | options = options }, Cmd.batch [ valueChanged (selectedOptionsToTuple options) ] )

                Err error ->
                    ( model, errorMessage (Json.Decode.errorToString error) )

        PlaceholderAttributeChanged newPlaceholder ->
            ( { model | placeholder = newPlaceholder }, Cmd.none )

        LoadingAttributeChanged bool ->
            ( { model
                | rightSlot =
                    updateRightSlotLoading
                        bool
                        model.selectionMode
                        (Option.hasSelectedOption model.options)
              }
            , Cmd.none
            )

        MaxDropdownItemsChanged int ->
            ( { model | maxDropdownItems = int }, Cmd.none )

        DisabledAttributeChanged bool ->
            ( { model | disabled = bool }, Cmd.none )

        SelectHighlightedOption ->
            let
                options =
                    selectHighlightedOption model.selectionMode model.options
            in
            case model.selectionMode of
                SingleSelect ->
                    ( { model | options = options, searchString = "" }
                    , Cmd.batch
                        [ valueChanged (selectedOptionsToTuple options)
                        , blurInput ()
                        ]
                    )

                MultiSelect ->
                    ( { model | options = options, searchString = "" }, valueChanged (selectedOptionsToTuple options) )

        DeleteInputForSingleSelect ->
            case model.selectionMode of
                SingleSelect ->
                    if Option.hasSelectedOption model.options then
                        ( { model
                            | options = Option.deselectAllOptionsInOptionsList model.options
                            , searchString = ""
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                MultiSelect ->
                    ( model, Cmd.none )

        EscapeKeyInInputFilter ->
            ( { model | searchString = "" }, blurInput () )

        MoveHighlightedOptionUp ->
            ( { model | options = Option.moveHighlightedOptionUp model.options }, Cmd.none )

        MoveHighlightedOptionDown ->
            ( { model | options = Option.moveHighlightedOptionDown model.options }, Cmd.none )

        ValueCasingWidthUpdate dims ->
            ( { model | valueCasingWidth = dims.width, valueCasingHeight = dims.height }, Cmd.none )

        ClearAllSelectedOptions ->
            ( { model
                | options = Option.deselectAllOptionsInOptionsList model.options
                , rightSlot = ShowNothing
              }
            , Cmd.batch [ valueChanged [] ]
            )

        ToggleSelectedValueHighlight optionValue ->
            ( { model | options = Option.toggleSelectedHighlightByOptionValue model.options optionValue }, Cmd.none )

        DeleteSelectedAndHighlightedValues ->
            let
                newOptions =
                    Option.deselectAllSelectedHighlightedOptions model.options
            in
            ( { model | options = newOptions }, valueChanged (selectedOptionsToTuple newOptions) )


updateRightSlot : RightSlot -> SelectionMode -> Bool -> RightSlot
updateRightSlot current selectionMode hasSelectedOption =
    case current of
        ShowNothing ->
            case selectionMode of
                SingleSelect ->
                    ShowDropdownIndicator

                MultiSelect ->
                    if hasSelectedOption then
                        ShowClearButton

                    else
                        ShowNothing

        ShowLoadingIndicator ->
            ShowLoadingIndicator

        ShowDropdownIndicator ->
            ShowDropdownIndicator

        ShowClearButton ->
            if hasSelectedOption then
                ShowClearButton

            else
                ShowNothing


updateRightSlotLoading : Bool -> SelectionMode -> Bool -> RightSlot
updateRightSlotLoading isLoading selectionMode hasSelectedOption =
    if isLoading then
        ShowLoadingIndicator

    else
        case selectionMode of
            SingleSelect ->
                ShowDropdownIndicator

            MultiSelect ->
                if hasSelectedOption then
                    ShowClearButton

                else
                    ShowNothing


view : Model -> Html Msg
view model =
    case model.selectionMode of
        SingleSelect ->
            let
                hasOptionSelected =
                    Option.hasSelectedOption model.options

                showPlaceholder =
                    not hasOptionSelected && not model.focused

                hasOptions =
                    List.isEmpty model.options |> not

                valueStr =
                    if hasOptionSelected then
                        model.options
                            |> Option.selectedOptionsToTuple
                            |> List.map Tuple.second
                            |> List.head
                            |> Maybe.withDefault ""

                    else if model.focused then
                        model.searchString

                    else
                        ""
            in
            div [ id "wrapper", css [ width (px model.valueCasingWidth) ] ]
                [ div
                    [ id "value-casing"
                    , onMouseDown BringInputInFocus
                    , onFocus BringInputInFocus
                    , tabindex 0
                    , classList
                        [ ( "placeholder", showPlaceholder )
                        , ( "value", hasOptionSelected )
                        , ( "single", True )
                        , ( "disabled", model.disabled )
                        ]
                    , css
                        [ if model.focused then
                            display none

                          else
                            display block
                        , width (px model.valueCasingWidth)
                        ]
                    ]
                    [ span
                        [ class "placeholder"
                        , css
                            [ if showPlaceholder then
                                display inline

                              else
                                display none
                            ]
                        ]
                        [ text model.placeholder ]
                    , span [ class "value" ] [ text valueStr ]
                    , input
                        [ type_ "text"
                        , onBlur InputBlur
                        , onFocus InputFocus
                        , onInput SearchInputOnInput
                        , value valueStr
                        , id "input-filter"
                        , disabled model.disabled
                        , css
                            [ if model.focused then
                                visibility visible

                              else
                                visibility hidden
                            , width (px model.valueCasingWidth)
                            ]
                        , Keyboard.on Keyboard.Keydown
                            [ ( Enter, SelectHighlightedOption )
                            , ( Backspace, DeleteInputForSingleSelect )
                            , ( Escape, EscapeKeyInInputFilter )
                            , ( ArrowUp, MoveHighlightedOptionUp )
                            , ( ArrowDown, MoveHighlightedOptionDown )
                            ]
                            |> Html.Styled.Attributes.fromUnstyled
                        ]
                        []
                    , case model.rightSlot of
                        ShowNothing ->
                            text ""

                        ShowLoadingIndicator ->
                            node "slot" [ name "loading-indicator" ] []

                        ShowDropdownIndicator ->
                            dropdownIndicator model.focused model.disabled hasOptions

                        ShowClearButton ->
                            node "slot" [ name "clear-button" ] []
                    , dropdown model
                    ]
                ]

        MultiSelect ->
            let
                hasOptionSelected =
                    Option.hasSelectedOption model.options

                showPlaceholder =
                    not hasOptionSelected && not model.focused

                inputFilter =
                    input
                        [ type_ "text"
                        , onBlur InputBlur
                        , onFocus InputFocus
                        , onInput SearchInputOnInput
                        , value model.searchString
                        , id "input-filter"
                        , disabled model.disabled
                        , Keyboard.on Keyboard.Keydown
                            [ ( Enter, SelectHighlightedOption )
                            , ( Escape, EscapeKeyInInputFilter )
                            , ( ArrowUp, MoveHighlightedOptionUp )
                            , ( ArrowDown, MoveHighlightedOptionDown )
                            ]
                            |> Html.Styled.Attributes.fromUnstyled
                        ]
                        []
            in
            div [ id "wrapper", classList [ ( "disabled", model.disabled ) ] ]
                [ div
                    [ id "value-casing"
                    , onClick BringInputInFocus
                    , onFocus BringInputInFocus
                    , Keyboard.on Keyboard.Keydown
                        [ ( Delete, DeleteSelectedAndHighlightedValues )
                        , ( Backspace, DeleteSelectedAndHighlightedValues )
                        ]
                        |> Html.Styled.Attributes.fromUnstyled
                    , tabindex 0
                    , classList
                        [ ( "placeholder", showPlaceholder )
                        , ( "multi", True )
                        , ( "disabled", model.disabled )
                        ]
                    , css
                        [ width (px model.valueCasingWidth)
                        ]
                    ]
                    ([ span
                        [ class "placeholder"
                        , css
                            [ if showPlaceholder then
                                display inline

                              else
                                display none
                            ]
                        ]
                        [ text model.placeholder ]
                     ]
                        ++ optionsToValuesHtml model.options
                        ++ [ inputFilter
                           , rightSlotHtml model.rightSlot model.focused model.disabled hasOptionSelected
                           ]
                    )
                , dropdown model
                ]


dropdownIndicator : Bool -> Bool -> Bool -> Html Msg
dropdownIndicator focused disabled hasOptions =
    if disabled || not hasOptions then
        text ""

    else if focused then
        div
            [ id "select-indicator"
            , mousedownPreventDefaultAndStopPropagation BringInputOutOfFocus
            , class "down"
            ]
            [ text "🔽" ]

    else
        div
            [ id "select-indicator"
            , mousedownPreventDefaultAndStopPropagation BringInputInFocus
            , class "up"
            ]
            [ text "🔼" ]


dropdown : Model -> Html Msg
dropdown model =
    let
        optionsHtml =
            optionsToDropdownOptions
                DropdownMouseOverOption
                DropdownMouseOutOption
                DropdownMouseClickOption
                model.selectionMode
                (OptionPresentor.prepareOptionsForPresentation model.maxDropdownItems model.searchString model.options)
    in
    if model.showDropdown && not (List.isEmpty model.options) && not (List.isEmpty optionsHtml) then
        div
            [ id "dropdown"
            , class "showing"
            , css [ top (px model.valueCasingHeight) ]
            ]
            optionsHtml

    else
        div [ id "dropdown", class "hiding", css [ top (px model.valueCasingHeight) ] ] optionsHtml


optionsToDropdownOptions :
    (OptionValue -> msg)
    -> (OptionValue -> msg)
    -> (OptionValue -> msg)
    -> SelectionMode
    -> List (OptionPresenter msg)
    -> List (Html msg)
optionsToDropdownOptions mouseOverMsgConstructor mouseOutMsgConstructor clickMsgConstructor selectionMode options =
    let
        partialWithSelectionMode =
            optionToDropdownOption
                mouseOverMsgConstructor
                mouseOutMsgConstructor
                clickMsgConstructor
                selectionMode

        helper : OptionGroup -> OptionPresenter msg -> ( OptionGroup, List (Html msg) )
        helper previousGroup option_ =
            ( option_.group
            , option_ |> partialWithSelectionMode (previousGroup /= option_.group)
            )
    in
    options
        |> mapAccuml helper Option.emptyOptionGroup
        |> Tuple.second
        |> List.concat
        |> List.filter (\htmlTag -> htmlTag /= text "")


optionToDropdownOption :
    (OptionValue -> msg)
    -> (OptionValue -> msg)
    -> (OptionValue -> msg)
    -> SelectionMode
    -> Bool
    -> OptionPresenter msg
    -> List (Html msg)
optionToDropdownOption mouseOverMsgConstructor mouseOutMsgConstructor clickMsgConstructor selectionMode prependOptionGroup option =
    let
        optionGroupHtml =
            if prependOptionGroup then
                div
                    [ class "optgroup"
                    ]
                    [ span [ class "optgroup-header" ] [ text (Option.optionGroupToString option.group) ] ]

            else
                text ""

        descriptionHtml : Html msg
        descriptionHtml =
            if OptionPresentor.hasDescription option then
                div
                    [ class "description"
                    ]
                    [ fromUnstyled option.descriptionMarkup ]

            else
                text ""
    in
    case option.display of
        OptionShown ->
            [ optionGroupHtml
            , div
                [ onMouseEnter (mouseOverMsgConstructor option.value)
                , onMouseLeave (mouseOutMsgConstructor option.value)
                , mousedownPreventDefaultAndStopPropagation (clickMsgConstructor option.value)
                , class "option"
                ]
                [ fromUnstyled option.labelMarkup, descriptionHtml ]
            ]

        OptionHidden ->
            [ optionGroupHtml, text "" ]

        OptionSelected ->
            case selectionMode of
                SingleSelect ->
                    [ optionGroupHtml
                    , div
                        [ class "selected"
                        , class "option"
                        ]
                        [ fromUnstyled option.labelMarkup, descriptionHtml ]
                    ]

                MultiSelect ->
                    [ optionGroupHtml, text "" ]

        OptionSelectedHighlighted ->
            case selectionMode of
                SingleSelect ->
                    [ optionGroupHtml
                    , div
                        [ class "selected"
                        , class "option"
                        ]
                        [ fromUnstyled option.labelMarkup, descriptionHtml ]
                    ]

                MultiSelect ->
                    [ optionGroupHtml, text "" ]

        OptionHighlighted ->
            [ optionGroupHtml
            , div
                [ onMouseEnter (mouseOverMsgConstructor option.value)
                , onMouseLeave (mouseOutMsgConstructor option.value)
                , mousedownPreventDefaultAndStopPropagation (clickMsgConstructor option.value)
                , class "highlighted"
                , class "option"
                ]
                [ fromUnstyled option.labelMarkup, descriptionHtml ]
            ]

        OptionDisabled ->
            [ optionGroupHtml
            , div
                [ class "disabled"
                , class "option"
                ]
                [ fromUnstyled option.labelMarkup, descriptionHtml ]
            ]


optionsToValuesHtml : List Option -> List (Html Msg)
optionsToValuesHtml options =
    options
        |> List.map
            (\option ->
                case option of
                    Option display (OptionLabel labelStr _) optionValue _ _ ->
                        case display of
                            OptionShown ->
                                text ""

                            OptionHidden ->
                                text ""

                            OptionSelected ->
                                div
                                    [ class "value"
                                    , mousedownPreventDefaultAndStopPropagation
                                        (ToggleSelectedValueHighlight optionValue)
                                    ]
                                    [ text labelStr ]

                            OptionSelectedHighlighted ->
                                div
                                    [ classList
                                        [ ( "value", True )
                                        , ( "selected-value", True )
                                        ]
                                    , mousedownPreventDefaultAndStopPropagation
                                        (ToggleSelectedValueHighlight optionValue)
                                    ]
                                    [ text labelStr ]

                            OptionHighlighted ->
                                text ""

                            OptionDisabled ->
                                text ""

                    EmptyOption display (OptionLabel labelStr _) ->
                        case display of
                            OptionShown ->
                                text ""

                            OptionHidden ->
                                text ""

                            OptionSelected ->
                                div [ class "value" ] [ text labelStr ]

                            OptionSelectedHighlighted ->
                                text ""

                            OptionHighlighted ->
                                text ""

                            OptionDisabled ->
                                text ""

                    CustomOption display (OptionLabel _ _) _ ->
                        case display of
                            _ ->
                                text ""
            )


rightSlotHtml : RightSlot -> Bool -> Bool -> Bool -> Html Msg
rightSlotHtml rightSlot focused disabled hasOptionSelected =
    case rightSlot of
        ShowNothing ->
            text ""

        ShowLoadingIndicator ->
            node "slot" [ name "loading-indicator" ] []

        ShowDropdownIndicator ->
            dropdownIndicator focused disabled hasOptionSelected

        ShowClearButton ->
            div
                [ id "clear-button-wrapper"
                , onClick ClearAllSelectedOptions
                ]
                [ node "slot"
                    [ name "clear-button"
                    , css
                        [ fontSize (px 30)
                        , lineHeight (px 30)
                        ]
                    ]
                    [ text "✕"
                    ]
                ]


showAddOption : Bool -> String -> Bool
showAddOption addable searchString =
    addable && searchString /= ""


type alias Flags =
    { value : String
    , placeholder : String
    , size : String
    , allowMultiSelect : Bool
    , optionsJson : String
    , loading : Bool
    , maxDropdownItems : Int
    , disabled : Bool
    , custom : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        selectionMode =
            if flags.allowMultiSelect then
                MultiSelect

            else
                SingleSelect

        initialValueStr =
            String.trim flags.value

        initialValues : List String
        initialValues =
            case initialValueStr of
                "" ->
                    []

                _ ->
                    case selectionMode of
                        SingleSelect ->
                            [ initialValueStr ]

                        MultiSelect ->
                            String.split "," initialValueStr

        ( optionsWithInitialValueSelected, errorCmd ) =
            case Json.Decode.decodeString Option.optionsDecoder flags.optionsJson of
                Ok options ->
                    case selectionMode of
                        SingleSelect ->
                            case List.head initialValues of
                                Just initialValueStr_ ->
                                    if Option.isOptionInListOfOptionsByValue (Option.stringToOptionValue initialValueStr_) options then
                                        ( Option.selectOptionsInOptionsListByString initialValues options, Cmd.none )

                                    else
                                        ( (Option.newOption initialValueStr_ Nothing |> Option.selectOption) :: options, Cmd.none )

                                Nothing ->
                                    ( options, Cmd.none )

                        MultiSelect ->
                            let
                                -- Don't include any empty options, that doesn't make sense.
                                optionsWithInitialValues =
                                    options
                                        |> List.filter (not << Option.isEmptyOption)
                                        |> Option.addAndSelectOptionsInOptionsListByString initialValues
                            in
                            ( optionsWithInitialValues, Cmd.none )

                Err error ->
                    ( [], errorMessage (Json.Decode.errorToString error) )
    in
    ( { initialValue = initialValues
      , placeholder = flags.placeholder
      , size = flags.size
      , selectionMode = selectionMode
      , options = optionsWithInitialValueSelected
      , showDropdown = False
      , searchString = ""
      , rightSlot =
            if flags.loading then
                ShowLoadingIndicator

            else
                case selectionMode of
                    SingleSelect ->
                        ShowDropdownIndicator

                    MultiSelect ->
                        if Option.hasSelectedOption optionsWithInitialValueSelected then
                            ShowClearButton

                        else
                            ShowNothing
      , maxDropdownItems = flags.maxDropdownItems
      , disabled = flags.disabled
      , focused = False

      -- TODO Should these be passed as flags?
      , valueCasingWidth = 100
      , valueCasingHeight = 45
      , custom = flags.custom
      }
    , errorCmd
    )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ valueChangedReceiver ValueChanged
        , addOptionsReceiver AddOptions
        , removeOptionsReceiver RemoveOptions
        , placeholderChangedReceiver PlaceholderAttributeChanged
        , loadingChangedReceiver LoadingAttributeChanged
        , disableChangedReceiver DisabledAttributeChanged
        , optionsChangedReceiver OptionsChanged
        , maxDropdownItemsChangedReceiver MaxDropdownItemsChanged
        , valueCasingDimensionsChangedReceiver ValueCasingWidthUpdate
        , selectOptionReceiver SelectOption
        , deselectOptionReceiver DeselectOption
        ]


mousedownPreventDefaultAndStopPropagation : a -> Html.Styled.Attribute a
mousedownPreventDefaultAndStopPropagation message =
    Html.Styled.Events.custom "mousedown"
        (Json.Decode.succeed
            { message = message
            , stopPropagation = True
            , preventDefault = True
            }
        )
