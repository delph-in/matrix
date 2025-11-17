

# General link to the delph-in matrix documentation:
MATRIX_DOC_LINK = "https://github.com/delph-in/docs/wiki/"

# Specific links to individual pages/libraries:
# When adding documentation pages/libraries, add the title_link = link_extension to this section!

ADNOMINALPOSSESSION_LINK = "MatrixDoc_AdnominalPossession"
ARGUMENTOPTIONALITY_LINK = "MatrixDoc_ArgumentOptionality"
CASE_LINK = "MatrixDoc_Case"
CLAUSALCOMPLEMENTS_LINK = "MatrixDoc_ClausalComplements"
CLAUSALMODIFIERS_LINK = "MatrixDoc_ClausalModifiers"
COORDINATION_LINK = "MatrixDoc_Coordination"
DIRECTINVERSE_LINK = "MatrixDoc_DirectInverse"
EVIDENTIALS_LINK = "MatrixDoc_Evidentials"
GENDER_LINK = "MatrixDoc_Gender"
GENERALINFO_LINK = "MatrixDoc_GeneralInfo"
INFORMATIONSTRUCTURE_LINK = "MatrixDoc_InformationStructure"
LEXICON_LINK = "MatrixDoc_Lexicon"
LEXICON_ADJECTIVES_LINK = "MatrixDoc_Lexicon#adjectives"
LEXICON_AUXILIARIES_LINK = "MatrixDoc_Lexicon#auxiliaries"
LEXICON_CASEMARKINGADPOSITIONS_LINK = "MatrixDoc_Lexicon#case-marking-adpositions"
LEXICON_COPULAS_LINK = "MatrixDoc_Lexicon#copulas"
LEXICON_DETERMINERS_LINK = "MatrixDoc_Lexicon#determiners"
LEXICON_NOUNS_LINK = "MatrixDoc_Lexicon#nouns"
LEXICON_VERBS_LINK = "MatrixDoc_Lexicon#verbs"
LIGHTVERBCONSTRUCTIONS_LINK = "MatrixDoc_LightVerbConstructions"
MORPHOLOGY_LINK = "MatrixDoc_Morphology"
NOMINALIZEDCLAUSES_LINK = "MatrixDoc_Nominalized-Clauses"
NUMBER_LINK = "MatrixDoc_Number"
OTHERFEATURES_LINK = "MatrixDoc_OtherFeatures"
PERSON_LINK = "MatrixDoc_Person"
SENTENTIALNEGATION_LINK = "MatrixDoc_SententialNegation"
TENSEASPECTMOOD_LINK = "MatrixDoc_TenseAspectMood"
WHQUESTIONS_LINK = "MatrixDoc_WhQuestions"
WORDORDER_LINK = "MatrixDoc_WordOrder"
YESNOQ_LINK = "MatrixDoc_YesNoQ"

# Method for creating a set of links from a set of matrix library extensions
# libraries = a list of link endings (strings); such as [CASE_LINK, GENDER_LINK]
# returns a set of full links (using a set to avoid repeats even though there shouldn't be any)
def set_links(libraries: list) -> set:
    links = set()
    for library in libraries:
        links.add(MATRIX_DOC_LINK + library)
    return links