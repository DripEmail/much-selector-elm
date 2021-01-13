module Option exposing
    ( Option(..)
    , OptionDescription(..)
    , OptionDisplay(..)
    , OptionGroup(..)
    , OptionLabel(..)
    , OptionValue(..)
    , addAdditionalOptionsToOptionList
    , addAndSelectOptionsInOptionsListByString
    , decoder
    , deselectAllOptionsInOptionsList
    , deselectAllSelectedHighlightedOptions
    , deselectOptionInListByOptionValue
    , emptyOptionGroup
    , getOptionDescription
    , getOptionDisplay
    , getOptionGroup
    , getOptionLabel
    , getOptionValue
    , hasSelectedOption
    , highlightOption
    , highlightOptionInListByValue
    , isEmptyOption
    , isOptionInListOfOptionsByValue
    , mergeTwoListsOfOptionsPreservingSelectedOptions
    , moveHighlightedOptionDown
    , moveHighlightedOptionUp
    , newDisabledOption
    , newOption
    , newSelectedOption
    , optionDescriptionToBool
    , optionDescriptionToSearchString
    , optionDescriptionToString
    , optionGroupToString
    , optionLabelToSearchString
    , optionLabelToString
    , optionsDecoder
    , removeHighlightOptionInList
    , removeOptionsFromOptionList
    , selectHighlightedOption
    , selectOption
    , selectOptionInListByOptionValue
    , selectOptionsInOptionsListByString
    , selectSingleOptionInList
    , selectedOptionsToTuple
    , setDescription
    , setDisplay
    , setGroup
    , setLabel
    , setSelectedOptionInNewOptions
    , stringToOptionValue
    , toggleSelectedHighlightByOptionValue
    , updateOrAddCustomOption
    )

import Json.Decode
import List.Extra
import SelectionMode exposing (SelectionMode(..))


type Option
    = Option OptionDisplay OptionLabel OptionValue OptionDescription OptionGroup
    | EmptyOption OptionDisplay OptionLabel
    | CustomOption OptionDisplay OptionLabel OptionValue


getOptionLabel : Option -> OptionLabel
getOptionLabel option =
    case option of
        Option _ label _ _ _ ->
            label

        EmptyOption _ label ->
            label

        CustomOption _ label _ ->
            label


type OptionDisplay
    = OptionShown
    | OptionHidden
    | OptionSelected
    | OptionSelectedHighlighted
    | OptionHighlighted
    | OptionDisabled


type OptionLabel
    = OptionLabel String (Maybe String)


optionLabelToString : OptionLabel -> String
optionLabelToString optionLabel =
    case optionLabel of
        OptionLabel label _ ->
            label


optionLabelToSearchString : OptionLabel -> String
optionLabelToSearchString optionLabel =
    case optionLabel of
        OptionLabel string maybeCleanString ->
            case maybeCleanString of
                Just cleanString ->
                    cleanString

                Nothing ->
                    String.toLower string


type OptionValue
    = OptionValue String
    | EmptyOptionValue


optionValueToString : OptionValue -> String
optionValueToString optionValue =
    case optionValue of
        OptionValue valueString ->
            valueString

        EmptyOptionValue ->
            ""


stringToOptionValue : String -> OptionValue
stringToOptionValue string =
    OptionValue string


type OptionDescription
    = OptionDescription String (Maybe String)
    | NoDescription


optionDescriptionToString : OptionDescription -> String
optionDescriptionToString optionDescription =
    case optionDescription of
        OptionDescription string _ ->
            string

        NoDescription ->
            ""


optionDescriptionToSearchString : OptionDescription -> String
optionDescriptionToSearchString optionDescription =
    case optionDescription of
        OptionDescription description maybeCleanDescription ->
            case maybeCleanDescription of
                Just cleanDescription ->
                    cleanDescription

                Nothing ->
                    String.toLower description

        NoDescription ->
            ""


optionDescriptionToBool : OptionDescription -> Bool
optionDescriptionToBool optionDescription =
    case optionDescription of
        OptionDescription _ _ ->
            True

        NoDescription ->
            False


type OptionGroup
    = OptionGroup String
    | NoOptionGroup


emptyOptionGroup : OptionGroup
emptyOptionGroup =
    NoOptionGroup


getOptionGroup : Option -> OptionGroup
getOptionGroup option =
    case option of
        Option _ _ _ _ optionGroup ->
            optionGroup

        CustomOption _ _ _ ->
            NoOptionGroup

        EmptyOption _ _ ->
            NoOptionGroup


newOption : String -> Maybe String -> Option
newOption value maybeCleanLabel =
    case value of
        "" ->
            EmptyOption OptionShown (OptionLabel "" maybeCleanLabel)

        _ ->
            Option OptionShown (OptionLabel value maybeCleanLabel) (OptionValue value) NoDescription NoOptionGroup


setLabel : String -> Maybe String -> Option -> Option
setLabel string maybeCleanString option =
    case option of
        Option optionDisplay _ optionValue description group ->
            Option optionDisplay (OptionLabel string maybeCleanString) optionValue description group

        EmptyOption optionDisplay _ ->
            EmptyOption optionDisplay (OptionLabel string maybeCleanString)

        CustomOption optionDisplay _ optionValue ->
            CustomOption optionDisplay (OptionLabel string maybeCleanString) optionValue


setValue : String -> Option -> Option
setValue string option =
    case option of
        Option optionDisplay optionLabel _ description group ->
            Option optionDisplay optionLabel (OptionValue string) description group

        CustomOption optionDisplay optionLabel _ ->
            CustomOption optionDisplay optionLabel (OptionValue string)

        EmptyOption optionDisplay optionLabel ->
            EmptyOption optionDisplay optionLabel


setDescription : String -> Option -> Option
setDescription string option =
    case option of
        Option optionDisplay label optionValue _ group ->
            Option optionDisplay label optionValue (OptionDescription string Nothing) group

        EmptyOption optionDisplay optionLabel ->
            EmptyOption optionDisplay optionLabel

        CustomOption optionDisplay optionLabel optionValue ->
            CustomOption optionDisplay optionLabel optionValue


setGroup : String -> Option -> Option
setGroup string option =
    case option of
        Option optionDisplay label optionValue description _ ->
            Option optionDisplay label optionValue description (OptionGroup string)

        EmptyOption optionDisplay optionLabel ->
            EmptyOption optionDisplay optionLabel

        CustomOption optionDisplay optionLabel optionValue ->
            CustomOption optionDisplay optionLabel optionValue


setOptionDisplay : OptionDisplay -> Option -> Option
setOptionDisplay optionDisplay option =
    case option of
        Option _ optionLabel optionValue optionDescription optionGroup ->
            Option optionDisplay optionLabel optionValue optionDescription optionGroup

        EmptyOption _ optionLabel ->
            EmptyOption optionDisplay optionLabel

        CustomOption _ optionLabel optionValue ->
            CustomOption optionDisplay optionLabel optionValue


setDisplay : OptionDisplay -> Option -> Option
setDisplay optionDisplay option =
    case option of
        Option _ label optionValue description group ->
            Option optionDisplay label optionValue description group

        CustomOption _ optionLabel optionValue ->
            CustomOption optionDisplay optionLabel optionValue

        EmptyOption _ optionLabel ->
            EmptyOption optionDisplay optionLabel


newSelectedOption : String -> Maybe String -> Option
newSelectedOption string maybeCleanLabel =
    Option OptionSelected (OptionLabel string maybeCleanLabel) (OptionValue string) NoDescription NoOptionGroup


newDisabledOption : String -> Maybe String -> Option
newDisabledOption string maybeCleanLabel =
    Option OptionDisabled (OptionLabel string maybeCleanLabel) (OptionValue string) NoDescription NoOptionGroup


getOptionDisplay : Option -> OptionDisplay
getOptionDisplay option =
    case option of
        Option display _ _ _ _ ->
            display

        EmptyOption display _ ->
            display

        CustomOption display _ _ ->
            display


getOptionValue : Option -> OptionValue
getOptionValue option =
    case option of
        Option _ _ value _ _ ->
            value

        EmptyOption _ _ ->
            EmptyOptionValue

        CustomOption _ _ value ->
            value


getOptionValueAsString : Option -> String
getOptionValueAsString option =
    case option |> getOptionValue of
        OptionValue string ->
            string

        EmptyOptionValue ->
            ""


optionGroupToString : OptionGroup -> String
optionGroupToString optionGroup =
    case optionGroup of
        OptionGroup string ->
            string

        NoOptionGroup ->
            ""


getOptionLabelString : Option -> String
getOptionLabelString option =
    case option of
        Option _ label _ _ _ ->
            optionLabelToString label

        CustomOption _ label _ ->
            optionLabelToString label

        EmptyOption _ label ->
            optionLabelToString label


getOptionDescription : Option -> OptionDescription
getOptionDescription option =
    case option of
        Option _ _ _ optionDescription _ ->
            optionDescription

        EmptyOption _ _ ->
            NoDescription

        CustomOption _ _ _ ->
            NoDescription


getOptionDescriptionString : Option -> String
getOptionDescriptionString option =
    case option |> getOptionDescription of
        OptionDescription string _ ->
            string

        NoDescription ->
            ""


optionHasDescription : Option -> Bool
optionHasDescription option =
    case option |> getOptionDescription of
        OptionDescription _ _ ->
            True

        NoDescription ->
            False


selectedOptionsToTuple : List Option -> List ( String, String )
selectedOptionsToTuple options =
    options |> selectedOptions |> List.map optionToValueLabelTuple


selectOptionsInOptionsListByString : List String -> List Option -> List Option
selectOptionsInOptionsListByString strings options =
    List.map
        (\option ->
            if isOptionValueInListOfStrings strings option then
                selectOption option

            else
                deselectOption option
        )
        options


addAndSelectOptionsInOptionsListByString : List String -> List Option -> List Option
addAndSelectOptionsInOptionsListByString strings options =
    let
        newOptions =
            List.map (\str -> newSelectedOption str Nothing) strings
    in
    mergeTwoListsOfOptionsPreservingSelectedOptions newOptions options


setSelectedOptionInNewOptions : List Option -> List Option -> List Option
setSelectedOptionInNewOptions oldOptions newOptions =
    let
        oldSelectedOption =
            oldOptions |> selectedOptions
    in
    List.map
        (\newOption_ ->
            if optionListContainsOptionWithValue newOption_ oldSelectedOption then
                selectOption newOption_

            else
                newOption_
        )
        newOptions


mergeTwoListsOfOptionsPreservingSelectedOptions : List Option -> List Option -> List Option
mergeTwoListsOfOptionsPreservingSelectedOptions optionsA optionsB =
    let
        superList =
            optionsA ++ optionsB

        newOptions =
            List.Extra.uniqueBy getOptionValueAsString superList
    in
    setSelectedOptionInNewOptions superList newOptions


deselectAllOptionsInOptionsList : List Option -> List Option
deselectAllOptionsInOptionsList options =
    List.map
        deselectOption
        options


isOptionValueInListOfStrings : List String -> Option -> Bool
isOptionValueInListOfStrings possibleValues option =
    List.any (\possibleValue -> getOptionValueAsString option == possibleValue) possibleValues


isOptionInListOfOptionsByValue : OptionValue -> List Option -> Bool
isOptionInListOfOptionsByValue optionValue options =
    List.any (\option -> (option |> getOptionValue |> optionValueToString) == (optionValue |> optionValueToString)) options


optionValuesEqual : Option -> OptionValue -> Bool
optionValuesEqual option optionValue =
    getOptionValue option == optionValue


highlightedOptionIndex : List Option -> Maybe Int
highlightedOptionIndex options =
    List.Extra.findIndex (\option -> optionIsHighlighted option) options


highlightOptionInList : Option -> List Option -> List Option
highlightOptionInList option options =
    List.map
        (\option_ ->
            if option == option_ then
                highlightOption option_

            else
                removeHighlightOption option_
        )
        options


highlightOptionInListByValue : OptionValue -> List Option -> List Option
highlightOptionInListByValue value options =
    List.map
        (\option_ ->
            if optionValuesEqual option_ value then
                highlightOption option_

            else
                removeHighlightOption option_
        )
        options


updateOrAddCustomOption : String -> List Option -> List Option
updateOrAddCustomOption string options =
    let
        options_ =
            List.Extra.dropWhile
                (\option_ ->
                    case option_ of
                        CustomOption _ _ _ ->
                            True

                        _ ->
                            False
                )
                options
    in
    [ CustomOption OptionShown (OptionLabel ("Add " ++ string ++ "...") Nothing) (OptionValue string) ] ++ options_


removeHighlightOptionInList : OptionValue -> List Option -> List Option
removeHighlightOptionInList value options =
    List.map
        (\option_ ->
            if optionValuesEqual option_ value then
                removeHighlightOption option_

            else
                option_
        )
        options


findClosestHighlightableOptionGoingUp : Int -> List Option -> Maybe Option
findClosestHighlightableOptionGoingUp index options =
    List.Extra.splitAt index options
        |> Tuple.first
        |> List.reverse
        |> List.Extra.find optionIsHighlightable


moveHighlightedOptionUp : List Option -> List Option
moveHighlightedOptionUp options =
    let
        maybeHigherSibling =
            options
                |> highlightedOptionIndex
                |> Maybe.andThen (\index -> findClosestHighlightableOptionGoingUp index options)
    in
    case maybeHigherSibling of
        Just option ->
            highlightOptionInList option options

        Nothing ->
            case List.head options of
                Just firstOption ->
                    highlightOptionInList firstOption options

                Nothing ->
                    options


findClosestHighlightableOptionGoingDown : Int -> List Option -> Maybe Option
findClosestHighlightableOptionGoingDown index options =
    List.Extra.splitAt index options
        |> Tuple.second
        |> List.Extra.find optionIsHighlightable


moveHighlightedOptionDown : List Option -> List Option
moveHighlightedOptionDown options =
    let
        maybeLowerSibling =
            options
                |> highlightedOptionIndex
                |> Maybe.andThen (\index -> findClosestHighlightableOptionGoingDown index options)
    in
    case maybeLowerSibling of
        Just option ->
            highlightOptionInList option options

        Nothing ->
            case List.head options of
                Just firstOption ->
                    highlightOptionInList firstOption options

                Nothing ->
                    options


selectOptionInListByOptionValue : OptionValue -> List Option -> List Option
selectOptionInListByOptionValue value options =
    List.map
        (\option_ ->
            if optionValuesEqual option_ value then
                selectOption option_

            else
                option_
        )
        options


deselectOptionInListByOptionValue : OptionValue -> List Option -> List Option
deselectOptionInListByOptionValue value options =
    List.map
        (\option_ ->
            if optionValuesEqual option_ value then
                deselectOption option_

            else
                option_
        )
        options


selectHighlightedOption : SelectionMode -> List Option -> List Option
selectHighlightedOption selectionMode options =
    options
        |> List.filter
            (\option ->
                optionIsHighlighted option
            )
        |> List.head
        |> (\maybeOption ->
                case maybeOption of
                    Just option ->
                        case option of
                            Option _ _ value _ _ ->
                                case selectionMode of
                                    MultiSelect ->
                                        selectOptionInListByOptionValue value options

                                    SingleSelect ->
                                        selectSingleOptionInList value options

                            EmptyOption _ _ ->
                                case selectionMode of
                                    MultiSelect ->
                                        selectEmptyOption options

                                    SingleSelect ->
                                        selectEmptyOption options

                            CustomOption _ _ value ->
                                case selectionMode of
                                    MultiSelect ->
                                        selectOptionInListByOptionValue value options

                                    SingleSelect ->
                                        selectSingleOptionInList value options

                    Nothing ->
                        options
           )


selectSingleOptionInList : OptionValue -> List Option -> List Option
selectSingleOptionInList value options =
    options
        |> List.map
            (\option_ ->
                if optionValuesEqual option_ value then
                    selectOption option_

                else
                    deselectOption option_
            )


selectEmptyOption : List Option -> List Option
selectEmptyOption options =
    options
        |> List.map
            (\option_ ->
                case option_ of
                    EmptyOption _ _ ->
                        selectOption option_

                    _ ->
                        deselectOption option_
            )


addAdditionalOptionsToOptionList : List Option -> List Option -> List Option
addAdditionalOptionsToOptionList currentOptions newOptions =
    List.filter (\new -> not (optionListContainsOptionWithValue new currentOptions)) newOptions
        ++ currentOptions


removeOptionsFromOptionList : List Option -> List Option -> List Option
removeOptionsFromOptionList options optionsToRemove =
    List.filter (\option -> not (optionListContainsOptionWithValue option optionsToRemove)) options


highlightOption : Option -> Option
highlightOption option =
    case option of
        Option display label value description group ->
            case display of
                OptionShown ->
                    Option OptionHighlighted label value description group

                OptionHidden ->
                    Option OptionHidden label value description group

                OptionSelected ->
                    Option OptionSelected label value description group

                OptionSelectedHighlighted ->
                    Option OptionSelectedHighlighted label value description group

                OptionHighlighted ->
                    Option OptionHighlighted label value description group

                OptionDisabled ->
                    Option OptionDisabled label value description group

        EmptyOption display label ->
            case display of
                OptionShown ->
                    EmptyOption OptionHighlighted label

                OptionHidden ->
                    EmptyOption OptionHidden label

                OptionSelected ->
                    EmptyOption OptionSelected label

                OptionSelectedHighlighted ->
                    EmptyOption OptionSelectedHighlighted label

                OptionHighlighted ->
                    EmptyOption OptionHighlighted label

                OptionDisabled ->
                    EmptyOption OptionDisabled label

        CustomOption display label value ->
            case display of
                OptionHidden ->
                    CustomOption OptionHidden label value

                OptionSelected ->
                    CustomOption OptionSelected label value

                OptionDisabled ->
                    CustomOption OptionDisabled label value

                _ ->
                    CustomOption OptionHighlighted label value


removeHighlightOption : Option -> Option
removeHighlightOption option =
    case option of
        Option display label value description group ->
            case display of
                OptionShown ->
                    Option OptionShown label value description group

                OptionHidden ->
                    Option OptionHidden label value description group

                OptionSelected ->
                    Option OptionSelected label value description group

                OptionSelectedHighlighted ->
                    Option OptionSelectedHighlighted label value description group

                OptionHighlighted ->
                    Option OptionShown label value description group

                OptionDisabled ->
                    Option OptionDisabled label value description group

        EmptyOption display label ->
            case display of
                OptionShown ->
                    EmptyOption OptionShown label

                OptionHidden ->
                    EmptyOption OptionHidden label

                OptionSelected ->
                    EmptyOption OptionSelected label

                OptionHighlighted ->
                    EmptyOption OptionShown label

                OptionDisabled ->
                    EmptyOption OptionDisabled label

                OptionSelectedHighlighted ->
                    EmptyOption OptionSelectedHighlighted label

        CustomOption display label value ->
            case display of
                OptionShown ->
                    CustomOption OptionShown label value

                OptionHidden ->
                    CustomOption OptionHidden label value

                OptionSelected ->
                    CustomOption OptionSelected label value

                OptionHighlighted ->
                    CustomOption OptionShown label value

                OptionDisabled ->
                    CustomOption OptionDisabled label value

                OptionSelectedHighlighted ->
                    CustomOption OptionSelectedHighlighted label value


optionIsHighlighted : Option -> Bool
optionIsHighlighted option =
    case option of
        Option display _ _ _ _ ->
            case display of
                OptionShown ->
                    False

                OptionHidden ->
                    False

                OptionSelected ->
                    False

                OptionSelectedHighlighted ->
                    False

                OptionHighlighted ->
                    True

                OptionDisabled ->
                    False

        EmptyOption display _ ->
            case display of
                OptionShown ->
                    False

                OptionHidden ->
                    False

                OptionSelected ->
                    False

                OptionSelectedHighlighted ->
                    False

                OptionHighlighted ->
                    True

                OptionDisabled ->
                    False

        CustomOption display _ _ ->
            case display of
                OptionHighlighted ->
                    True

                _ ->
                    False


optionIsHighlightable : Option -> Bool
optionIsHighlightable option =
    case option of
        Option display _ _ _ _ ->
            case display of
                OptionShown ->
                    True

                OptionHidden ->
                    False

                OptionSelected ->
                    False

                OptionSelectedHighlighted ->
                    False

                OptionHighlighted ->
                    False

                OptionDisabled ->
                    False

        EmptyOption display _ ->
            case display of
                OptionShown ->
                    True

                OptionHidden ->
                    False

                OptionSelected ->
                    False

                OptionSelectedHighlighted ->
                    False

                OptionHighlighted ->
                    False

                OptionDisabled ->
                    False

        CustomOption display _ _ ->
            case display of
                OptionShown ->
                    True

                _ ->
                    False


selectOption : Option -> Option
selectOption option =
    case option of
        Option display label value description group ->
            case display of
                OptionShown ->
                    Option OptionSelected label value description group

                OptionHidden ->
                    Option OptionSelected label value description group

                OptionSelected ->
                    Option OptionSelected label value description group

                OptionSelectedHighlighted ->
                    Option OptionSelected label value description group

                OptionHighlighted ->
                    Option OptionSelected label value description group

                OptionDisabled ->
                    Option OptionDisabled label value description group

        EmptyOption display label ->
            case display of
                OptionShown ->
                    EmptyOption OptionSelected label

                OptionHidden ->
                    EmptyOption OptionSelected label

                OptionSelected ->
                    EmptyOption OptionSelected label

                OptionSelectedHighlighted ->
                    EmptyOption OptionSelected label

                OptionHighlighted ->
                    EmptyOption OptionSelected label

                OptionDisabled ->
                    EmptyOption OptionDisabled label

        CustomOption display _ value ->
            let
                label =
                    OptionLabel (getOptionValueAsString option) Nothing
            in
            case display of
                OptionShown ->
                    Option OptionSelected label value NoDescription NoOptionGroup

                OptionHidden ->
                    Option OptionSelected label value NoDescription NoOptionGroup

                OptionSelected ->
                    Option OptionSelected label value NoDescription NoOptionGroup

                OptionHighlighted ->
                    Option OptionSelected label value NoDescription NoOptionGroup

                OptionDisabled ->
                    Option OptionDisabled label value NoDescription NoOptionGroup

                OptionSelectedHighlighted ->
                    Option OptionSelected label value NoDescription NoOptionGroup


deselectOption : Option -> Option
deselectOption option =
    case option of
        Option display label value description group ->
            case display of
                OptionShown ->
                    Option OptionShown label value description group

                OptionHidden ->
                    Option OptionHidden label value description group

                OptionSelected ->
                    Option OptionShown label value description group

                OptionSelectedHighlighted ->
                    Option OptionShown label value description group

                OptionHighlighted ->
                    Option OptionHighlighted label value description group

                OptionDisabled ->
                    Option OptionDisabled label value description group

        EmptyOption display label ->
            case display of
                OptionShown ->
                    EmptyOption OptionShown label

                OptionHidden ->
                    EmptyOption OptionHidden label

                OptionSelected ->
                    EmptyOption OptionShown label

                OptionSelectedHighlighted ->
                    EmptyOption OptionShown label

                OptionHighlighted ->
                    EmptyOption OptionHighlighted label

                OptionDisabled ->
                    EmptyOption OptionDisabled label

        CustomOption display label value ->
            case display of
                OptionHidden ->
                    CustomOption OptionHidden label value

                OptionHighlighted ->
                    CustomOption OptionHighlighted label value

                OptionDisabled ->
                    CustomOption OptionDisabled label value

                _ ->
                    CustomOption OptionShown label value


selectedOptions : List Option -> List Option
selectedOptions options =
    options
        |> List.filter
            (\option_ ->
                case option_ of
                    Option display _ _ _ _ ->
                        case display of
                            OptionShown ->
                                False

                            OptionHidden ->
                                False

                            OptionSelected ->
                                True

                            OptionSelectedHighlighted ->
                                True

                            OptionHighlighted ->
                                False

                            OptionDisabled ->
                                False

                    EmptyOption display _ ->
                        case display of
                            OptionShown ->
                                False

                            OptionHidden ->
                                False

                            OptionSelected ->
                                True

                            OptionSelectedHighlighted ->
                                True

                            OptionHighlighted ->
                                False

                            OptionDisabled ->
                                False

                    CustomOption display _ _ ->
                        case display of
                            OptionSelected ->
                                True

                            OptionHighlighted ->
                                False

                            OptionSelectedHighlighted ->
                                True

                            _ ->
                                False
            )


toggleSelectedHighlightByOptionValue : List Option -> OptionValue -> List Option
toggleSelectedHighlightByOptionValue options optionValue =
    options
        |> List.map
            (\option_ ->
                case option_ of
                    Option optionDisplay _ optionValue_ _ _ ->
                        if optionValue == optionValue_ then
                            case optionDisplay of
                                OptionShown ->
                                    option_

                                OptionHidden ->
                                    option_

                                OptionSelected ->
                                    option_ |> setOptionDisplay OptionSelectedHighlighted

                                OptionSelectedHighlighted ->
                                    option_ |> setOptionDisplay OptionSelected

                                OptionHighlighted ->
                                    option_

                                OptionDisabled ->
                                    option_

                        else
                            option_

                    EmptyOption _ _ ->
                        option_

                    CustomOption optionDisplay _ optionValue_ ->
                        if optionValue == optionValue_ then
                            case optionDisplay of
                                OptionSelected ->
                                    option_ |> setOptionDisplay OptionSelectedHighlighted

                                OptionSelectedHighlighted ->
                                    option_ |> setOptionDisplay OptionSelected

                                _ ->
                                    option_

                        else
                            option_
            )


deselectAllSelectedHighlightedOptions : List Option -> List Option
deselectAllSelectedHighlightedOptions options =
    options
        |> List.map
            (\option_ ->
                case option_ of
                    Option optionDisplay _ _ _ _ ->
                        case optionDisplay of
                            OptionShown ->
                                option_

                            OptionHidden ->
                                option_

                            OptionSelected ->
                                option_

                            OptionSelectedHighlighted ->
                                option_ |> setOptionDisplay OptionShown

                            OptionHighlighted ->
                                option_

                            OptionDisabled ->
                                option_

                    EmptyOption _ _ ->
                        option_

                    CustomOption optionDisplay _ _ ->
                        case optionDisplay of
                            OptionSelectedHighlighted ->
                                option_ |> setOptionDisplay OptionShown

                            _ ->
                                option_
            )


hasSelectedOption : List Option -> Bool
hasSelectedOption options =
    options |> selectedOptions |> List.isEmpty |> not


isEmptyOption : Option -> Bool
isEmptyOption option =
    case option of
        EmptyOption _ _ ->
            True

        _ ->
            False


optionListContainsOptionWithValue : Option -> List Option -> Bool
optionListContainsOptionWithValue option options =
    let
        optionValue =
            getOptionValue option
    in
    List.filter (\option_ -> getOptionValue option_ == optionValue) options
        |> List.isEmpty
        |> not


optionToValueLabelTuple : Option -> ( String, String )
optionToValueLabelTuple option =
    ( getOptionValueAsString option, getOptionLabel option |> optionLabelToString )


optionsDecoder : Json.Decode.Decoder (List Option)
optionsDecoder =
    Json.Decode.list decoder


decoder : Json.Decode.Decoder Option
decoder =
    Json.Decode.oneOf
        [ decodeOptionWithoutAValue
        , decodeOptionWithAValue
        ]


decodeOptionWithoutAValue : Json.Decode.Decoder Option
decodeOptionWithoutAValue =
    Json.Decode.field
        "value"
        valueDecoder
        |> Json.Decode.andThen
            (\value ->
                case value of
                    OptionValue _ ->
                        Json.Decode.fail "It can not be an option without a value because it has a value."

                    EmptyOptionValue ->
                        Json.Decode.map2
                            EmptyOption
                            displayDecoder
                            labelDecoder
            )


decodeOptionWithAValue : Json.Decode.Decoder Option
decodeOptionWithAValue =
    Json.Decode.map5 Option
        displayDecoder
        labelDecoder
        (Json.Decode.field
            "value"
            valueDecoder
        )
        descriptionDecoder
        optionGroupDecoder


displayDecoder : Json.Decode.Decoder OptionDisplay
displayDecoder =
    Json.Decode.oneOf
        [ Json.Decode.field
            "selected"
            Json.Decode.string
            |> Json.Decode.andThen
                (\str ->
                    case str of
                        "true" ->
                            Json.Decode.succeed OptionSelected

                        _ ->
                            Json.Decode.fail "Option is not selected"
                )
        , Json.Decode.field
            "disabled"
            Json.Decode.bool
            |> Json.Decode.andThen
                (\isDisabled ->
                    if isDisabled then
                        Json.Decode.succeed OptionDisabled

                    else
                        Json.Decode.fail "Option is not disabled"
                )
        , Json.Decode.succeed OptionShown
        ]


labelDecoder : Json.Decode.Decoder OptionLabel
labelDecoder =
    Json.Decode.map2
        OptionLabel
        (Json.Decode.field "label" Json.Decode.string)
        (Json.Decode.field "labelClean" (Json.Decode.nullable Json.Decode.string))


valueDecoder : Json.Decode.Decoder OptionValue
valueDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\valueStr ->
                case String.trim valueStr of
                    "" ->
                        Json.Decode.succeed EmptyOptionValue

                    str ->
                        Json.Decode.succeed (OptionValue str)
            )


descriptionDecoder : Json.Decode.Decoder OptionDescription
descriptionDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map2 OptionDescription
            (Json.Decode.field "description" Json.Decode.string)
            (Json.Decode.field "descriptionClean" (Json.Decode.nullable Json.Decode.string))
        , Json.Decode.succeed NoDescription
        ]


optionGroupDecoder : Json.Decode.Decoder OptionGroup
optionGroupDecoder =
    Json.Decode.oneOf
        [ Json.Decode.field "group" Json.Decode.string
            |> Json.Decode.map OptionGroup
        , Json.Decode.succeed NoOptionGroup
        ]
