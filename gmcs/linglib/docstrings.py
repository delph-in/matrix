
#AVERY:
DOCSTRING_STARTER = "This type as generated from the customization system bore constraints from these libraries:\n"

# General link to the delph-in matrix documentation:
MATRIX_DOC_LINK = "https://delph-in.github.io/docs/matrix/"


# Specific links to individual pages/libraries:
# When adding documentation pages/libraries, add the title:link-ending pair to this dictionary!
PAGE_LINKS = {
    "adnominalpossession":"MatrixDoc_AdnominalPossession/",
    "argumentoptionality":"MatrixDoc_ArgumentOptionality/",
    "case":"MatrixDoc_Case/",
    "clausalcomplements":"MatrixDoc_ClausalComplements/",
    "clausalmodifiers":"MatrixDoc_ClausalModifiers/",
    "coordination":"MatrixDoc_Coordination/",
    "directinverse":"MatrixDoc_DirectInverse/",
    "evidentials":"MatrixDoc_Evidentials/",
    "gender":"MatrixDoc_Gender/",
    "generalinfo":"MatrixDoc_GeneralInfo/",
    "informationstructure":"MatrixDoc_InformationStructure/",
    "lexicon":"MatrixDoc_Lexicon/",
    "morphology":"MatrixDoc_Morphology/",
    "nominalizedclauses":"MatrixDoc_NominalizedClauses/",
    "number":"MatrixDoc_Number/",
    "otherfeatures":"MatrixDoc_OtherFeatures/",
    "person":"MatrixDoc_Person/",
    "sententialnegation":"MatrixDoc_SententialNegation/",
    "tenseaspectmood":"MatrixDoc_TenseAspectMood/",
    "whquestions":"MatrixDoc_WhQuestions/",
    "wordorder":"MatrixDoc_WordOrder/",
    "yesnoq":"MatrixDoc_YesNoQ/"

}

# Method for creating a docstring
# Parameters: list of documentation page names
# Returns: docstring with introduction and links
def link_to(libraries: list, note="") -> str:
    links = []
    for library in libraries:
        library = library.replace(" ", "")
        links.append(MATRIX_DOC_LINK + PAGE_LINKS[library.lower()])

    if note != "":
        note = " (" + note + ")"

    return DOCSTRING_STARTER + "\n".join(links) + note

def add_links(libraries: list, note="") -> str:
    links = []
    for library in libraries:
        library = library.replace(" ", "")
        links.append(MATRIX_DOC_LINK + PAGE_LINKS[library.lower()])
    if note != "":
        note = " (" + note + ")"
    return "\n".join(links) + note