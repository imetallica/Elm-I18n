module I18n exposing (Translation, translateTo)


type alias Translation a = (a, (String, String))

{-| This function transforms a text in one language to a text in another language.
It will ignore cases where it can't find the correct value.

    translateTo Portuguese [(Portuguese, ("Mommy", "Mamãe"))] "Mommy" = "Mamãe"
-}

translateTo : a -> List (Translation a) -> String -> String
translateTo lang translations text =
  let
    maybeTranslatedText = List.head (List.filterMap (findTranslation lang text) translations)
  in
    case maybeTranslatedText of
      Just translatedText -> translatedText
      Nothing -> text

findTranslation : a -> String -> Translation a -> Maybe String
findTranslation lang text translation =
  if lang == (fst translation) then
    if text == (fst (snd translation)) then
      Just (snd (snd translation))
    else Nothing
  else Nothing
