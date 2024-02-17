Lab 06 - Regular Expressions and Web Scraping
================
Jing Yu
Feb 16, 2024

# Learning goals

- Use a real world API to make queries and process the data.
- Use regular expressions to parse the information.
- Practice your GitHub skills.

# Lab description

In this lab, we will be working with the [NCBI
API](https://www.ncbi.nlm.nih.gov/home/develop/api/) to make queries and
extract information using XML and regular expressions. For this lab, we
will be using the `httr`, `xml2`, and `stringr` R packages.

This markdown document should be rendered using `github_document`
document ONLY and pushed to your *JSC370-labs* repository in
`lab06/README.md`.

## Question 1: How many sars-cov-2 papers?

Build an automatic counter of sars-cov-2 papers using PubMed. You will
need to apply XPath as we did during the lecture to extract the number
of results returned by PubMed in the following web address:

    https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2

Complete the lines of code:

``` r
# Downloading the website
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2")

# Finding the counts
counts <- xml2::xml_find_first(website, "//span[@class='value']")

# Turning it into text
counts <- as.character(counts)

# Extracting the data using regex
stringr::str_extract(counts, "[0-9,]{7}")
```

    ## [1] "218,851"

- How many sars-cov-2 papers are there?

*Answer here.*

There are 218,851 sars-cov-2 papers.

Don’t forget to commit your work!

## Question 2: Academic publications on COVID19 related to Toronto

Use the function `httr::GET()` to make the following query:

1.  Baseline URL:
    <https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi>

2.  Query parameters:

    - db: pubmed
    - term: covid19 toronto
    - retmax: 300

The parameters passed to the query are documented
[here](https://www.ncbi.nlm.nih.gov/books/NBK25499/).

``` r
library(httr)
query_ids <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    db="pubmed", term="covid19 toronto", retmax=300
    )
)

# Extracting the content of the response of GET
ids <- httr::content(query_ids)
```

The query will return an XML object, we can turn it into a character
list to analyze the text directly with `as.character()`. Another way of
processing the data could be using lists with the function
`xml2::as_list()`. We will skip the latter for now.

Take a look at the data, and continue with the next question (don’t
forget to commit and push your results to your GitHub repo!).

## Question 3: Get details about the articles

The Ids are wrapped around text in the following way:
`<Id>... id number ...</Id>`. we can use a regular expression that
extract that information. Fill out the following lines of code:

``` r
# Turn the result into a character vector
ids <- as.character(ids)

# Find all the ids 
ids <- stringr::str_extract_all(ids, "<Id>[0-9]+</Id>")[[1]]

# Remove all the leading and trailing <Id> </Id>. Make use of "|"
ids <- stringr::str_remove_all(ids, "<Id>|</Id>")
```

With the ids in hand, we can now try to get the abstracts of the papers.
As before, we will need to coerce the contents (results) to a list
using:

1.  Baseline url:
    <https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi>

2.  Query parameters:

    - db: pubmed
    - id: A character with all the ids separated by comma, e.g.,
      “1232131,546464,13131”
    - retmax: 300
    - rettype: abstract

**Pro-tip**: If you want `GET()` to take some element literal, wrap it
around `I()` (as you would do in a formula in R). For example, the text
`"123,456"` is replaced with `"123%2C456"`. If you don’t want that
behavior, you would need to do the following `I("123,456")`.

``` r
publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
  query = list(
    db="pubmed", id=paste(ids, collapse=','), retmax=300, rettype="abstract"
    )
)

# Turning the output into character vector
publications <- httr::content(publications)
publications_txt <- as.character(publications)
```

With this in hand, we can now analyze the data. This is also a good time
for committing and pushing your work!

## Question 4: Distribution of universities, schools, and departments

Using the function `stringr::str_extract_all()` applied on
`publications_txt`, capture all the terms of the form:

1.  University of …
2.  … Institute of …

Write a regular expression that captures all such instances

``` r
institution <- stringr::str_extract_all(
  publications_txt,
  "(University of \\w+)|([A-Z]* Institute of \\w+)"
  ) 
institution <- unlist(institution)
as.data.frame(table(institution))
```

    ##                       institution Freq
    ## 1        Institute of Acupuncture    1
    ## 2            Institute of Applied    1
    ## 3          Institute of Arthritis    1
    ## 4         Institute of Biophysics    4
    ## 5      Institute of Biostatistics    1
    ## 6           Institute of Business    1
    ## 7           Institute of Clinical    1
    ## 8           Institute of Critical    1
    ## 9           Institute of Diabetes    1
    ## 10         Institute of Engineers    1
    ## 11           Institute of General    1
    ## 12          Institute of Genetics    1
    ## 13            Institute of Global    2
    ## 14            Institute of Health   36
    ## 15            Institute of Higher    1
    ## 16          Institute of Manitoba    1
    ## 17           Institute of Medical   14
    ## 18            Institute of Mental    1
    ## 19           Institute of Network    1
    ## 20      Institute of Neurological    1
    ## 21   Institute of Neuropsychiatry    1
    ## 22      Institute of Neuroscience    1
    ## 23          Institute of Oncology    1
    ## 24        Institute of Psychology    3
    ## 25           Institute of Science    4
    ## 26            Institute of Social    1
    ## 27                Institute of St    4
    ## 28        Institute of Technology    5
    ## 29               Institute of the    5
    ## 30          Institute of Training    1
    ## 31     CIHR Institute of Genetics    1
    ## 32       CIHR Institute of Health    1
    ## 33         IZA Institute of Labor    1
    ## 34         University of Aberdeen    1
    ## 35         University of Adelaide    2
    ## 36          University of alberta    1
    ## 37          University of Alberta   65
    ## 38        University of Amsterdam    2
    ## 39        University of Antioquia    1
    ## 40          University of Applied    1
    ## 41          University of Arizona    4
    ## 42         University of Auckland    7
    ## 43        University of Barcelona    2
    ## 44             University of Bari    5
    ## 45            University of Basel    2
    ## 46           University of Beirut    1
    ## 47         University of Belgrade    3
    ## 48           University of Bergen    3
    ## 49           University of Berlin    2
    ## 50             University of Bern    2
    ## 51       University of Birmingham    3
    ## 52             University of Bonn    1
    ## 53          University of Brescia   18
    ## 54          University of Bristol    1
    ## 55          University of British  119
    ## 56          University of Calgary   57
    ## 57       University of California   12
    ## 58        University of Cambridge    5
    ## 59         University of Campinas    1
    ## 60             University of Cape    1
    ## 61        University of Cartagena    1
    ## 62          University of Chicago    2
    ## 63          University of Cologne    4
    ## 64         University of Colorado    1
    ## 65      University of Connecticut    5
    ## 66       University of Copenhagen    1
    ## 67             University of Doha    3
    ## 68          University of Eastern    2
    ## 69           University of Exeter    1
    ## 70          University of Florida    1
    ## 71           University of Foggia    2
    ## 72           University of Gdansk    1
    ## 73           University of Geneva    2
    ## 74          University of Glasgow   13
    ## 75          University of Granada    3
    ## 76        University of Groningen    1
    ## 77           University of Guelph    9
    ## 78            University of Halle    1
    ## 79           University of Hawaii    1
    ## 80           University of Health    1
    ## 81         University of Helsinki    3
    ## 82    University of Hertfordshire    1
    ## 83             University of Hong   16
    ## 84         University of Illinois    6
    ## 85           University of Kansas    4
    ## 86             University of Kent    2
    ## 87          University of Koblenz    2
    ## 88             University of Kyiv    2
    ## 89                University of L    1
    ## 90            University of Leeds    2
    ## 91           University of Lisbon    1
    ## 92        University of Liverpool    1
    ## 93        University of Ljubljana    1
    ## 94           University of London    1
    ## 95         University of Manitoba   23
    ## 96         University of Mannheim    1
    ## 97         University of Maryland    1
    ## 98          University of Mashhad    2
    ## 99          University of Medical   22
    ## 100        University of Medicine    2
    ## 101       University of Melbourne    4
    ## 102         University of Messina    2
    ## 103          University of Mexico    1
    ## 104        University of Michigan    7
    ## 105           University of Milan    1
    ## 106          University of Milano    5
    ## 107         University of Mineiro    1
    ## 108          University of Modena    1
    ## 109     University of Montpellier    1
    ## 110        University of Montreal    6
    ## 111        University of Montréal    5
    ## 112          University of Mostar    1
    ## 113         University of Navarra    1
    ## 114           University of Negev    1
    ## 115             University of New    4
    ## 116       University of Newcastle    1
    ## 117    University of Newfoundland    2
    ## 118         University of Nigeria    1
    ## 119           University of North    4
    ## 120        University of Northern    1
    ## 121          University of Norway    1
    ## 122           University of Notre    1
    ## 123      University of Nottingham    2
    ## 124          University of Oregon    2
    ## 125            University of Oslo    1
    ## 126          University of Ottawa   59
    ## 127          University of Oxford   14
    ## 128           University of Padua    1
    ## 129         University of Paraiba   13
    ## 130         University of Pelotas    3
    ## 131    University of Pennsylvania   13
    ## 132      University of Pittsburgh   12
    ## 133        University of Plymouth    1
    ## 134        University of Pretoria    1
    ## 135          University of Public    1
    ## 136          University of Punjab    1
    ## 137      University of Queensland    6
    ## 138          University of Regina    2
    ## 139             University of Rio    6
    ## 140       University of Rochester    1
    ## 141            University of Rome    7
    ## 142             University of São    1
    ## 143    University of Saskatchewan    5
    ## 144         University of Seville    4
    ## 145      University of Sherbrooke    1
    ## 146       University of Singapore    5
    ## 147           University of South    4
    ## 148        University of Southern    7
    ## 149          University of Sydney   46
    ## 150          University of Tehran    1
    ## 151           University of Texas    3
    ## 152             University of the    2
    ## 153       University of Timisoara    3
    ## 154         University of Toronto  854
    ## 155          University of Trento    1
    ## 156            University of Utah    4
    ## 157          University of Verona    1
    ## 158        University of Victoria   14
    ## 159          University of Vienna    3
    ## 160        University of Virginia    1
    ## 161         University of Waikato    1
    ## 162          University of Warsaw    2
    ## 163      University of Washington    7
    ## 164        University of Waterloo   15
    ## 165         University of Western   16
    ## 166         University of Windsor    1
    ## 167       University of Wisconsin    1
    ## 168   University of Witwatersrand    1
    ## 169       University of Wuppertal    2
    ## 170            University of York    2
    ## 171          University of Zurich    5

Repeat the exercise and this time focus on schools and departments in
the form of

1.  School of …
2.  Department of …

And tabulate the results

``` r
schools_and_deps <- stringr::str_extract_all(
  publications_txt,
  "(School of \\w+)|(Department of \\w+)"
  )
as.data.frame(table(schools_and_deps))
```

    ##                      schools_and_deps Freq
    ## 1              Department of Academic    1
    ## 2          Department of Agricultural    1
    ## 3           Department of Agriculture    1
    ## 4           Department of Anaesthesia    2
    ## 5       Department of Anaesthesiology    2
    ## 6               Department of Anatomy    1
    ## 7            Department of Anesthesia   17
    ## 8        Department of Anesthesiology    8
    ## 9          Department of Anthropology    1
    ## 10              Department of Applied    3
    ## 11         Department of Biochemistry   14
    ## 12            Department of Bioethics    1
    ## 13           Department of Biological    4
    ## 14              Department of Biology    8
    ## 15           Department of Biomedical    7
    ## 16        Department of Biostatistics    8
    ## 17        Department of Biotechnology    2
    ## 18               Department of Cancer    3
    ## 19           Department of Cardiology    2
    ## 20       Department of Cardiovascular    1
    ## 21                 Department of Cell    3
    ## 22            Department of Chemistry    1
    ## 23             Department of Children    1
    ## 24             Department of Clinical   29
    ## 25            Department of Cognition    1
    ## 26            Department of Cognitive    1
    ## 27            Department of Community   21
    ## 28             Department of Computer    6
    ## 29           Department of Continuity    1
    ## 30             Department of Criminal    1
    ## 31          Department of Criminology    1
    ## 32             Department of Critical   29
    ## 33              Department of Defence    1
    ## 34          Department of Dermatology   20
    ## 35        Department of Developmental    1
    ## 36           Department of Diagnostic    6
    ## 37                 Department of Drug    1
    ## 38              Department of Dynamic    2
    ## 39            Department of Economics    1
    ## 40            Department of Education    8
    ## 41           Department of Electrical    4
    ## 42            Department of Emergency   39
    ## 43        Department of Environmental    3
    ## 44         Department of Epidemiology   28
    ## 45         Department of Experimental    7
    ## 46               Department of Family  114
    ## 47              Department of General    1
    ## 48            Department of Geography    3
    ## 49           Department of Geriatrics    2
    ## 50               Department of Global    7
    ## 51           Department of Government    1
    ## 52           Department of Gynecology    1
    ## 53               Department of Health   75
    ## 54           Department of Healthcare    1
    ## 55                Department of Heath    1
    ## 56             Department of Homeland    1
    ## 57                Department of Human    2
    ## 58           Department of Humanities    2
    ## 59           Department of Immunology   21
    ## 60           Department of Infectious    2
    ## 61          Department of Information    1
    ## 62           Department of Integrated    1
    ## 63             Department of Internal   19
    ## 64        Department of International    3
    ## 65              Department of Justice    1
    ## 66          Department of Kinesiology    3
    ## 67           Department of Laboratory    9
    ## 68                 Department of Life    1
    ## 69          Department of Mathematics   10
    ## 70              Department of Medical   32
    ## 71             Department of Medicine  174
    ## 72               Department of Mental    1
    ## 73         Department of Microbiology   12
    ## 74            Department of Molecular   13
    ## 75             Department of National    4
    ## 76           Department of Nephrology    9
    ## 77      Department of Neuroimmunology    1
    ## 78            Department of Neurology   29
    ## 79      Department of Neuropsychiatry    1
    ## 80         Department of Neuroscience    2
    ## 81         Department of Neurosurgery    3
    ## 82                  Department of Non    1
    ## 83              Department of Nursing    2
    ## 84            Department of Nutrition    1
    ## 85          Department of Nutritional   11
    ## 86           Department of Obstetrics   31
    ## 87         Department of Occupational    9
    ## 88             Department of Oncology    9
    ## 89            Department of Optometry    1
    ## 90         Department of Orthopaedics    5
    ## 91       Department of Otolaryngology    2
    ## 92  Department of Otorhinolaryngology    1
    ## 93           Department of Paediatric    2
    ## 94          Department of Paediatrics   98
    ## 95           Department of Palliative    1
    ## 96         Department of Paramedicine    1
    ## 97            Department of Pathology   47
    ## 98            Department of Pediatric    5
    ## 99           Department of Pediatrics   80
    ## 100        Department of Periodontics    2
    ## 101      Department of Pharmaceutical    2
    ## 102        Department of Pharmacology   15
    ## 103            Department of Pharmacy    1
    ## 104          Department of Philosophy    2
    ## 105            Department of Physical   22
    ## 106             Department of Physics    1
    ## 107          Department of Physiology    2
    ## 108       Department of Physiotherapy    3
    ## 109           Department of Political    7
    ## 110            Department of Politics    1
    ## 111          Department of Population    7
    ## 112          Department of Preventive    6
    ## 113             Department of Primary    1
    ## 114        Department of Propedeutics    1
    ## 115          Department of Psychiatry  159
    ## 116       Department of Psychological    5
    ## 117          Department of Psychology   56
    ## 118        Department of Psychosocial    1
    ## 119              Department of Public   10
    ## 120           Department of Radiation   20
    ## 121           Department of Radiology    3
    ## 122      Department of Rehabilitation    1
    ## 123        Department of Reproductive    1
    ## 124            Department of Research    7
    ## 125         Department of Respiratory    1
    ## 126        Department of Rheumatology    1
    ## 127             Department of Service    1
    ## 128              Department of Social    7
    ## 129           Department of Sociology    4
    ## 130           Department of Somnology    1
    ## 131               Department of South    1
    ## 132               Department of Spine    4
    ## 133          Department of Statistics    3
    ## 134          Department of Supportive    6
    ## 135             Department of Surgery   20
    ## 136             Department of Systems    4
    ## 137          Department of Toxicology    1
    ## 138       Department of Translational    1
    ## 139            Department of Veterans    1
    ## 140          Department of Veterinary    1
    ## 141                  School of Allied   12
    ## 142                   School of Basic   10
    ## 143            School of Biodiversity    6
    ## 144              School of Biomedical    1
    ## 145                School of Business    5
    ## 146                  School of Cancer    2
    ## 147          School of Cardiovascular    2
    ## 148                School of Clinical   12
    ## 149                 School of Coimbra    1
    ## 150               School of Community    1
    ## 151                    School of Data    1
    ## 152               School of Dentistry    6
    ## 153               School of Economics    9
    ## 154             School of Educational    2
    ## 155             School of Engineering    5
    ## 156            School of Epidemiology   11
    ## 157               School of Geography    2
    ## 158                  School of Global    6
    ## 159                School of Graduate    1
    ## 160                  School of Health   21
    ## 161                 School of Hygiene    8
    ## 162           School of International    1
    ## 163              School of Journalism    1
    ## 164             School of Kinesiology    2
    ## 165                    School of Life   11
    ## 166              School of Management    6
    ## 167                School of Medicine  143
    ## 168               School of Molecular    1
    ## 169                 School of Nursing   17
    ## 170            School of Occupational    8
    ## 171                School of Pharmacy    1
    ## 172                School of Physical    5
    ## 173           School of Physiotherapy    1
    ## 174              School of Population   24
    ## 175                 School of Primary    2
    ## 176              School of Psychiatry    1
    ## 177              School of Psychology    5
    ## 178                  School of Public  168
    ## 179          School of Rehabilitation   14
    ## 180                  School of Social    8
    ## 181                     School of the    2

## Question 5: Form a database

We want to build a dataset which includes the title and the abstract of
the paper. The title of all records is enclosed by the HTML tag
`ArticleTitle`, and the abstract by `AbstractText`.

Before applying the functions to extract text directly, it will help to
process the XML a bit. We will use the `xml2::xml_children()` function
to keep one element per id. This way, if a paper is missing the
abstract, or something else, we will be able to properly match PUBMED
IDS with their corresponding records.

``` r
pub_char_list <- xml2::xml_children(publications)
pub_char_list <- sapply(pub_char_list, as.character)
```

Now, extract the abstract and article title for each one of the elements
of `pub_char_list`. You can either use `sapply()` as we just did, or
simply take advantage of vectorization of `stringr::str_extract`

``` r
abstracts <- stringr::str_extract(pub_char_list, "<AbstractText>.*</AbstractText>")
abstracts <- stringr::str_remove_all(abstracts, "<AbstractText>|</AbstractText>")
abstracts <- stringr::str_remove_all(abstracts, "'^\\s+|\\s+$'")
# alternatively, you can also use str_replace_all(abstracts, "[ORIGINAL]", "[REPLACEMENT]")
print(length(abstracts[is.na(abstracts)]))
```

    ## [1] 188

- How many of these don’t have an abstract?

*Answer here.*

There are 188 articles without an abstract.

Now, the title

``` r
titles <- stringr::str_extract(pub_char_list, "<ArticleTitle>.*</ArticleTitle>")
titles <- stringr::str_remove_all(titles, "<ArticleTitle>|</ArticleTitle>")
print(length(abstracts[is.na(titles)]))
```

    ## [1] 0

- How many of these don’t have a title ?

*Answer here.*

There is no article without a title.

Finally, put everything together into a single `data.frame` and use
`knitr::kable` to print the results

``` r
database <- data.frame(
  abstracts, titles
)
database <- knitr::kable(database)
kableExtra::scroll_box(database, width="500px", height="1000px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:1000px; overflow-x: scroll; width:500px; ">

<table>
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
abstracts
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
titles
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Proteomics of serum-derived extracellular vesicles are associated with
the severity and different clinical profiles of patients with COVID-19:
An exploratory secondary analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
The Canadian Sentinel Practitioner Surveillance Network reports
mid-season 2023/24 influenza vaccine effectiveness (VE) of 63% (95% CI:
51-72) against influenza A(H1N1)pdm09, lower for clade 5a.2a.1 (56%;
95% CI: 33-71) than clade 5a.2a (67%; 95% CI: 48-80), and lowest against
influenza A(H3N2) (40%; 95% CI: 5-61). The Omicron XBB.1.5 vaccine
protected comparably well, with VE of 47% (95% CI: 21-65) against
medically attended COVID-19, higher among people reporting a prior
confirmed SARS-CoV-2 infection at 67% (95% CI: 28-85).
</td>
<td style="text-align:left;">
2023/24 mid-season influenza and Omicron XBB.1.5 vaccine effectiveness
estimates from the Canadian Sentinel Practitioner Surveillance Network
(SPSN).
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Correction: Examining the impact of COVID-19 on health care utilization
among persons with chronic spinal cord injury/dysfunction: a population
study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Phenome-wide analysis reveals epistatic associations between APOL1
variants and chronic kidney disease and multiple other disorders.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Self-care interventions to assist family physicians with mental health
care of older patients during the COVID-19 pandemic: Feasibility,
acceptability, and outcomes of a pilot randomized controlled trial.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Risk of Arterial and Venous Thrombotic Events Among Patients with
COVID-19: A Multi-National Collaboration of Regulatory Agencies from
Canada, Europe, and United States.
</td>
</tr>
<tr>
<td style="text-align:left;">
The ongoing wars in many regions-such as the conflict between Israel and
Hamas-as well as the effects of war on communities, social services, and
mental health are covered in this special editorial. This article
emphasizes the need for international efforts to promote peace, offer
humanitarian aid, and address the mental health challenges faced by
individuals and communities affected by war and violence.
</td>
<td style="text-align:left;">
Harbingers of Hope: Scientists and the Pursuit of World Peace.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Resource use and disease severity of children hospitalized for COVID-19
versus multisystem inflammatory syndrome in children (MIS-C) in Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
We report a case of bilateral posterior ischaemic optic neuropathy,
which followed vaccination with ChAdOx1 nCoV-19 for COVID-19
prophylaxis. A man in his early 60s was initially assessed for bilateral
acute vision loss following 3 days of frontal headaches. The patient
denied any other preceding visual concerns or symptoms of giant cell
arteritis. The patient received his first dose of the ChAdOx1 nCoV-19
vaccination 10 days before the onset of his symptoms.At initial
presentation, visual acuity was counting fingers bilaterally. Blood work
found normal erythrocyte sedimentation rate (6 mm/hour) and C reactive
protein (&lt;5 mg/L) as well as a negative infectious and autoimmune
serology. He was negative for COVID-19 with PCR testing.
Diffusion-weighted MRI showed restricted diffusion along both optic
nerves. After 5 months, the patient’s visual acuity remained counting
fingers bilaterally with pale optic nerves. Isolated bilateral posterior
ischaemic optic neuropathy has not been reported in association with the
ChAdOx1 nCoV-19 vaccine.
</td>
<td style="text-align:left;">
Bilateral non-arteritic posterior ischaemic optic neuropathy following
COVID-19 vaccination.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
COVID-19 vaccines and adverse events of special interest: A
multinational Global Vaccine Data Network (GVDN) cohort study of 99
million vaccinated individuals.
</td>
</tr>
<tr>
<td style="text-align:left;">
Previous studies have shown significant sex-specific differences in
major depressive disorder (MDD) in multiple biological parameters. Most
studies focused on young and middle-aged adults, and there is a paucity
of information about sex-specific biological differences in older adults
with depression (aka, late-life depression (LLD)). To address this gap,
this study aimed to evaluate sex-specific biological abnormalities in a
large group of individuals with LLD using an untargeted proteomic
analysis. We quantified 344 plasma proteins using a multiplex assay in
430 individuals with LLD and 140 healthy comparisons (HC) (age range
between 60 and 85 years old for both groups). Sixty-six signaling
proteins were differentially expressed in LLD (both sexes). Thirty-three
proteins were uniquely associated with LLD in females, while six
proteins were uniquely associated with LLD in males. The main biological
processes affected by these proteins in females were related to
immunoinflammatory control. In contrast, despite the smaller number of
associated proteins, males showed dysregulations in a broader range of
biological pathways, including immune regulation pathways, cell cycle
control, and metabolic control. Sex has a significant impact on
biomarker changes in LLD. Despite some overlap in differentially
expressed biomarkers, males and females show different patterns of
biomarkers changes, and males with LLD exhibit abnormalities in a larger
set of biological processes compared to females. Our findings can
provide novel targets for sex-specific interventions in LLD.
</td>
<td style="text-align:left;">
Sex differences in plasma proteomic markers in late-life depression.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
A global comparative analysis of the the inclusion of priority setting
in national COVID-19 pandemic plans: A reflection on the methods and the
accessibility of the plans.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of Delayed Dental Treatment during the COVID-19 Pandemic in an
Undergraduate Dental Clinic in Southwestern Ontario, Canada - A
Retrospective Chart Review.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Pre- and post-COVID-19 gender trends in authorship for paediatric
radiology articles worldwide: a systematic review.
</td>
</tr>
<tr>
<td style="text-align:left;">
Coronavirus disease 2019 (COVID-19) has been very challenging for those
living with overweight and obesity. The magnitude of this impact on
sleep requires further attention to optimise patient care and outcomes.
This study assessed the impact of the COVID-19 lockdown on sleep
duration and quality as well as identify predictors of poor sleep
quality in individuals with reported diagnoses of obstructive sleep
apnoea and those without sleep apnoea. An online survey (June-October
2020) was conducted with two samples; one representative of Canadians
living with overweight and obesity (n = 1089) and a second of
individuals recruited through obesity clinical services or patient
organisations (n = 980). While overall sleep duration did not decline
much, there were identifiable groups with reduced or increased sleep.
Those with changed sleep habits, especially reduced sleep, had much
poorer sleep quality, were younger, gained more weight and were more
likely to be female. Poor sleep quality was associated with medical,
social and eating concerns as well as mood disturbance. Those with sleep
apnoea had poorer quality sleep although this was offset to some degree
by use of CPAP. Sleep quality and quantity has been significantly
impacted during the early part of the COVID-19 pandemic in those living
with overweight and obesity. Predictors of poor sleep and the impact of
sleep apnoea with and without CPAP therapy on sleep parameters has been
evaluated. Identifying those at increased risk of sleep alterations and
its impact requires further clinical consideration.
</td>
<td style="text-align:left;">
Impact of COVID-19 pandemic on sleep parameters and characteristics in
individuals living with overweight and obesity.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The global impact of the COVID-19 pandemic on pediatric spinal care: A
multi-centric study.
</td>
</tr>
<tr>
<td style="text-align:left;">
\<b\>Background:\</b\> COVID-19-related global health governance (GHG)
processes and public health measures taken influenced population health
priorities worldwide. We investigated the intersection between
COVID-19-related GHG and how it redefined population health priorities
in Canada and other G20 countries. We analysed a Canada-related
multilevel qualitative study and a scoping review of selected G20
countries. Findings show the importance of linking equity considerations
to funding and accountability when responding to COVID-19. Nationalism
and limited coordination among governance actors contributed to
fragmented COVID-19 public health responses. COVID-19-related
consequences were not systematically negative, but when they were, they
affected more population groups living and working in conditions of
vulnerability and marginalisation. \<b\>Policy options and
recommendations:\</b\> Six policy options are proposed addressing
upstream determinants of health, such as providing sufficient funding
for equitable and accountable global and public health outcomes and
implementing gender-focused policies to reduce COVID-19 response-related
inequities and negative consequences downstream. Specific programmatic
(e.g., assessing the needs of the community early) and research
recommendations are also suggested to redress identified gaps.
\<b\>Conclusion:\</b\> Despite the consequences of the COVID-19
pandemic, programmatic and research opportunities along with concrete
policy options must be mobilised and implemented without further delay.
We collectively share the duty to act upon global health justice.
</td>
<td style="text-align:left;">
The Intersections of COVID-19 Global Health Governance and Population
Health Priorities: Equity-Related Lessons Learned From Canada and
Selected G20 Countries.
</td>
</tr>
<tr>
<td style="text-align:left;">
High levels of COVID-19 vaccine hesitancy have been reported among Black
and Latinx populations, with lower vaccination coverage among racialized
versus White sexual and gender minorities. We examined multilevel
contexts that influence COVID-19 vaccine uptake, barriers to
vaccination, and vaccine hesitancy among predominantly racialized sexual
and gender minority individuals. Semi-structured online interviews
explored perspectives and experiences around COVID-19 vaccination.
Interviews were recorded, transcribed, uploaded into ATLAS.ti, and
reviewed using thematic analysis. Among 40 participants (mean age,
29.0 years \[SD, 9.6\]), all identified as sexual and/or gender
minority, 82.5% of whom were racialized. COVID-19 vaccination
experiences were dominated by structural barriers: systemic racism,
transphobia and homophobia in healthcare and government/public health
institutions; limited availability of vaccination/appointments in
vulnerable neighborhoods; absence of culturally-tailored and
multi-language information; lack of digital/internet access; and
prohibitive indirect costs of vaccination. Vaccine hesitancy reflected
in uncertainties about a novel vaccine amid conflicting information and
institutional mistrust was integrally linked to structural factors.
Findings suggest that the uncritical application of “vaccine hesitancy”
to unilaterally explain undervaccination among marginalized populations
risks conflating structural and institutional barriers with
individual-level psychological factors, in effect placing the onus on
those most disenfranchised to overcome societal and institutional
processes of marginalization. Rather, disaggregating structural
determinants of vaccination availability, access, and institutional
stigma and mistrust from individual attitudes and decision-making that
reflect vaccine hesitancy, may support 1) evidence-informed
interventions to mitigate structural barriers in access to vaccination,
and 2) culturally-informed approaches to address decisional ambivalence
in the context of structural homophobia, transphobia, and racism.
</td>
<td style="text-align:left;">
\<i\>“Going vaccine hunting”\</i\>: Multilevel influences on COVID-19
vaccination among racialized sexual and gender minority adults-a
qualitative study.
</td>
</tr>
<tr>
<td style="text-align:left;">
\<b\>Background\</b\> Since the outbreak of COVID-19, how to reduce the
risk of spreading viruses and other microorganisms while performing
aerosol generating procedures (AGPs) has become a challenging question
within the dental and dental hygiene communities. The purpose of this
position paper is to summarize the existing evidence about the
effectiveness of various mitigation methods used to reduce the risk of
infection transmission during AGPs in dentistry.\<b\>Methods\</b\> The
authors searched six databases, MEDLINE, EMBASE, Scopus, Web of Science,
Cochrane Library, and Google Scholar, for relevant scientific evidence
published in the last ten years (January 2012 to December 2022) to
answer six research questions about the the aspects of risk of
transmission, methods, devices, and personal protective equipment (PPE)
used to reduce contact with microbial pathogens and limit the spread of
aerosols.\<b\>Results\</b\> A total of 78 studies fulfilled the
eligibility criteria. There was limited literature to indicate the risk
of infection transmission of SARS-CoV-2 between dental hygienists and
their patients. A number of mouthrinses are effective in reducing
bacterial contaminations in aerosols; however, their effectiveness
against SARS-CoV-2 was limited. The combined use of eyewear, masks, and
face shields are effective for the prevention of contamination of the
facial and nasal region, while performing AGPs. High volume evacuation
with or without an intraoral suction, low volume evacuation, saliva
ejector, and rubber dam (when appropriate) have shown effectiveness in
reducing aerosol transmission beyond the generation site. Finally, the
appropriate combination of ventilation and filtration in dental
operatories are effective in limiting the spread of
aerosols.\<b\>Conclusion\</b\> Aerosols produced during clinical
procedures can potentially pose a risk of infection transmission between
dental hygienists and their patients. The implementation of practices
supported by available evidence are best practices to ensure patient and
provider safety in oral health settings. More studies in dental clinical
environment would shape future practices and protocols, ultimately to
ensure safe clinical care delivery.
</td>
<td style="text-align:left;">
Aerosol Generating Procedures and Associated Control/Mitigation
Measures: A position paper from the Canadian Dental Hygienists
Association and the American Dental Hygienists’ Association.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Prevalence and drivers of nurse and physician distress in cardiovascular
and oncology programmes at a Canadian quaternary hospital network during
the COVID-19 pandemic: a quality improvement initiative.
</td>
</tr>
<tr>
<td style="text-align:left;">
In Canada, long-term care and retirement home residents have experienced
high rates of COVID-19 infection and death. Early efforts to protect
residents included restricting all visitors as well as movement inside
homes. These restrictions, however, had significant implications for
residents’ health and well-being. Engaging with those most affected by
such restrictions can help us to better understand their experiences and
address their needs. In this qualitative study, 43 residents of
long-term care or retirement homes, family members and staff were
interviewed and offered recommendations related to infection control,
communication, social contact and connection, care needs, and policy and
planning. The recommendations were examined using an ethical framework,
providing potential relevance in policy development for public health
crises. Our results highlight the harms of movement and visiting
restrictions and call for effective, equitable, and transparent
measures. The design of long-term care and retirement policies requires
ongoing, meaningful engagement with those most affected.
</td>
<td style="text-align:left;">
Recommendations Related to Visitor and Movement Restrictions in
Long-Term Care and Retirement Homes in Ontario during the COVID-19
Pandemic: Perspectives of Residents, Families, and Staff.
</td>
</tr>
<tr>
<td style="text-align:left;">
Even though Canada is one of the most culturally diverse countries in
the world, offering universal healthcare to all its citizens, the recent
COVID-19 pandemic has exposed significant health inequities in our
healthcare system. We continue to face challenges ensuring health equity
in cardiovascular diseases. Persistent outcome disparities may be driven
by race and ethnicity-based differences in healthcare delivery,
treatment, follow-up, and outcomes. However, we lack data about these
processes because they are not routinely collected. There are
significant opportunities to implement sustainable processes to collect
data that can generate evidence to inform and deliver equitable
healthcare and address racial and ethnic disparities in cardiovascular
disease.
</td>
<td style="text-align:left;">
fRace and Ethnicity Research in Cardiovascular Disease in Canada:
Challenges and Opportunities.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of Telehealth Post-operative Care on Early Outcomes Following
Esophagectomy.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Evaluation of an automated matching system of children and families to
virtual mental health resources during COVID-19.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Serologic Response to Vaccine for COVID-19 in Patients with Hematologic
Malignancy: A Prospective Cohort Study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The impact of public health lockdown measures during the COVID-19
pandemic on the epidemiology of children’s orthopedic injuries requiring
operative intervention.
</td>
</tr>
<tr>
<td style="text-align:left;">
RéSUMé: CONTEXTE: Les soins virtuels au Canada ont rapidement pris de
l’ampleur pendant la pandémie de COVID-19 dans un environnement où les
règles sont peu strictes, en réponse aux besoins urgents d’accès continu
aux soins dans un contexte de restrictions en santé publique. Les
spécialistes de la médecine d’urgence sont maintenant confrontés au défi
de conseiller sur les services de soins d’urgence virtuels qui devraient
rester dans le cadre des soins d’urgence complets. Il faut tenir compte
des soins sécuritaires, de qualité et appropriés, ainsi que des
questions d’accès équitable, de la demande publique et de la durabilité
(financière et autre). L’objectif de ce projet était de résumer la
littérature actuelle et l’opinion d’experts et de formuler des
recommandations sur la voie à suivre pour les soins virtuels en médecine
d’urgence. MéTHODES: Nous avons formé un groupe de travail composé de
médecins urgentistes de partout au Canada qui travaillent dans divers
milieux de pratique. Le groupe de travail sur les soins virtuels a
effectué un examen de la portée de la documentation et s’est réuni
chaque mois pour discuter des thèmes et formuler des recommandations.
Les recommandations finales ont été distribuées aux intervenants pour
obtenir leurs commentaires, puis présentées au symposium universitaire
2023 de l’Association canadienne des médecins d’urgence (ACMU) pour
discussion, rétroaction et perfectionnement. RéSULTATS: Le groupe de
travail a élaboré et atteint l’unanimité sur neuf recommandations
portant sur les thèmes de la conception du système, de l’équité et de
l’accessibilité, de la qualité et de la sécurité des patients, de
l’éducation et des programmes, des modèles financiers et de la viabilité
des services virtuels de soins d’urgence au Canada. CONCLUSION : Les
soins d’urgence virtuels sont devenus un service établi dans le système
de santé canadien. Les spécialistes en médecine d’urgence sont
particulièrement bien placés pour fournir un leadership et des conseils
sur la prestation optimale de ces services afin d’améliorer et de
compléter les soins d’urgence au Canada.
</td>
<td style="text-align:left;">
Virtual urgent care is here to stay: driving toward safe, equitable, and
sustainable integration within emergency medicine.
</td>
</tr>
<tr>
<td style="text-align:left;">
\<b\>Background:\</b\> Healthcare workers (HCWs) across the globe have
reported symptoms of Post-Traumatic Stress Disorder (PTSD) during the
COVID-19 pandemic. Moral Injury (MI) has been associated with PTSD in
military populations, but is not well studied in healthcare contexts.
Moral Distress (MD), a related concept, may enhance understandings of MI
and its relation to PTSD among HCWs. This study examined the independent
and combined impact of MI and MD on PTSD symptoms in Canadian HCWs
during the pandemic.\<b\>Methods:\</b\> HCWs participated in an online
survey between February and December 2021, with questions regarding
sociodemographics, mental health and trauma history (e.g. MI, MD, PTSD,
dissociation, depression, anxiety, stress, childhood adversity).
Structural equation modelling was used to analyze the independent and
combined impact of MI and MD on PTSD symptoms (including dissociation)
among the sample when controlling for sex, age, depression, anxiety,
stress, and childhood adversity.\<b\>Results:\</b\> A structural
equation model independently regressing both MI and MD onto PTSD
accounted for 74.4% of the variance in PTSD symptoms. Here, MI was
strongly and significantly associated with PTSD symptoms
(\<i\>β\</i\> = .412, \<i\>p\</i\> &lt; .0001) to a higher degree than
MD (\<i\>β\</i\> = .187, \<i\>p\</i\> &lt; .0001), after controlling for
age, sex, depression, anxiety, stress and childhood adversity. A model
regressing a combined MD and MI construct onto PTSD predicted
approximately 87% of the variance in PTSD symptoms
(\<i\>r\</i\>\<sup\>2\</sup\> = .87, \<i\>p\</i\> &lt; .0001), with
MD/MI strongly and significantly associated with PTSD
(\<i\>β\</i\> = .813, \<i\>p\</i\> &lt; .0001), after controlling for
age, sex, depression, anxiety, stress, and childhood
adversity.\<b\>Conclusion:\</b\> Our results support a relation between
MI and PTSD among HCWs and suggest that a combined MD and MI construct
is most strongly associated with PTSD symptoms. Further research is
needed better understand the mechanisms through which MD/MI are
associated with PTSD.
</td>
<td style="text-align:left;">
The independent and combined impact of moral injury and moral distress
on post-traumatic stress disorder symptoms among healthcare workers
during the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
\<b\>Introduction\</b\>: Healthcare workers (HCWs) often experience
morally challenging situations in their workplaces that may contribute
to job turnover and compromised well-being. This study aimed to
characterize the nature and frequency of moral stressors experienced by
HCWs during the COVID-19 pandemic, examine their influence on
psychosocial-spiritual factors, and capture the impact of such factors
and related moral stressors on HCWs’ self-reported job attrition
intentions.\<b\>Methods:\</b\> A sample of 1204 Canadian HCWs were
included in the analysis through a web-based survey platform whereby
work-related factors (e.g. years spent working as HCW, providing care to
COVID-19 patients), moral distress (captured by MMD-HP), moral injury
(captured by MIOS), mental health symptomatology, and job turnover due
to moral distress were assessed.\<b\>Results:\</b\> Moral stressors with
the highest reported frequency and distress ratings included patient
care requirements that exceeded the capacity HCWs felt safe/comfortable
managing, reported lack of resource availability, and belief that
administration was not addressing issues that compromised patient care.
Participants who considered leaving their jobs (44%; \<i\>N \</i\>= 517)
demonstrated greater moral distress and injury scores. Logistic
regression highlighted burnout (AOR = 1.59; \<i\>p\</i\> &lt; .001),
moral distress (AOR = 1.83; \<i\>p\</i\> &lt; .001), and moral injury
due to trust violation (AOR = 1.30; \<i\>p \</i\>= .022) as significant
predictors of the intention to leave one’s job.\<b\>Conclusion:\</b\>
While it is impossible to fully eliminate moral stressors from
healthcare, especially during exceptional and critical scenarios like a
global pandemic, it is crucial to recognize the detrimental impacts on
HCWs. This underscores the urgent need for additional research to
identify protective factors that can mitigate the impact of these
stressors.
</td>
<td style="text-align:left;">
Exposure to moral stressors and associated outcomes in healthcare
workers: prevalence, correlates, and impact on job attrition.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
A systematic review of studies on stress during the COVID-19 pandemic by
visualizing their structure through COOC, VOS viewer, and Cite Space
software.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Anti-Black discrimination in primary health care: a qualitative study
exploring internalized racism in a Canadian context.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Role of modifiable organisational factors in decreasing barriers to
mental healthcare: a longitudinal study of mission meaningfulness, team
relatedness and leadership trust among Canadian military personnel
deployed on Operation LASER.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Vaccination Recommendations for Adults Receiving Biologics and Oral
Therapies for Psoriasis and Psoriatic Arthritis: Delphi Consensus from
the Medical Board of the National Psoriasis Foundation.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Glucagon and GLP-1 receptor dual agonist survodutide for obesity: a
randomised, double-blind, placebo-controlled, dose-finding phase 2
trial.
</td>
</tr>
<tr>
<td style="text-align:left;">
Elevated psychological distress, experienced by pregnant women and
parents, has been well-documented during the COVID-19 pandemic. Most
research focuses on the first 6-months postpartum, with single or
limited repeated measures of perinatal distress. The present
longitudinal study examined how perinatal distress, experienced over
nearly 2 years of the COVID-19 pandemic, impacted toddler socioemotional
development. A sample of 304 participants participated during pregnancy,
6-weeks, 6-months, and 15-months postpartum. Mothers reported their
depressive, anxiety, and stress symptoms, at each timepoint.
Mother-reported toddler socioemotional functioning (using the Brief
Infant-Toddler Social and Emotional Assessment) was measured at
15-months. Results of structural equation mediation models indicated
that (1) higher prenatal distress was associated with elevated
postpartum distress, from 6-weeks to 15-months postpartum; (2)
associations between prenatal distress and toddler socioemotional
problems became nonsignificant after accounting for postpartum distress;
and (3) higher prenatal distress was indirectly associated with greater
socioemotional problems, and specifically elevated externalizing
problems, through higher maternal distress at 6 weeks and 15 months
postpartum. Findings suggest that the continued experience of distress
during the postpartum period plays an important role in child
socioemotional development during the COVID-19 pandemic.
</td>
<td style="text-align:left;">
A longitudinal study examining the associations between prenatal and
postnatal maternal distress and toddler socioemotional developmental
during the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Estimating the population effectiveness of interventions against
COVID-19 in France: A modelling study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Characteristics of the sexual networks of gay, bisexual, and other men
who have sex with men in Montréal, Toronto, and Vancouver: implications
for the transmission and control of mpox in Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Economic evaluations of treatment of depressive disorders in
adolescents: Protocol for a scoping review.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Prevalence and factors associated with depression and anxiety among
COVID-19 survivors in Dhaka city.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of vortioxetine on psychosocial functioning moderated by symptoms
of fatigue in post-COVID-19 condition: a secondary analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
Built Environment Testing for SARS-CoV-2Wastewater testing has proven to
be a valuable tool for forecasting Covid-19 outbreaks. Fralick
et al. now report that swabbing of surfaces (i.e., floors) for
SARS-CoV-2 may provide a similar benefit for predicting outbreaks in
long-term care homes.
</td>
<td style="text-align:left;">
Detection of Covid-19 Outbreaks Using Built Environment Testing for
SARS-CoV-2.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Long-term safety, tolerability, and efficacy of efgartigimod (ADAPT+):
interim results from a phase 3 open-label extension study in
participants with generalized myasthenia gravis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Changes in breakfast and water consumption among adolescents in Canada:
examining the impact of COVID-19 in worsening inequity.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Variation in occupational exposure risk for COVID-19 workers’
compensation claims across pandemic waves in Ontario.
</td>
</tr>
<tr>
<td style="text-align:left;">
Flashbulb memories (FBMs) refer to vivid and long-lasting
autobiographical memories for the circumstances in which people learned
of a shocking and consequential public event. A cross-national study
across eleven countries aimed to investigate FBM formation following the
first COVID-19 case news in each country and test the effect of
pandemic-related variables on FBM. Participants had detailed memories of
the date and others present when they heard the news, and had partially
detailed memories of the place, activity, and news source. China had the
highest FBM specificity. All countries considered the COVID-19 emergency
as highly significant at both the individual and global level. The
Classification and Regression Tree Analysis revealed that FBM
specificity might be influenced by participants’ age, subjective
severity (assessment of COVID-19 impact in each country and relative to
others), residing in an area with stringent COVID-19 protection
measures, and expecting the pandemic effects. Hierarchical regression
models demonstrated that age and subjective severity negatively
predicted FBM specificity, whereas sex, pandemic impact expectedness,
and rehearsal showed positive associations in the total sample.
Subjective severity negatively affected FBM specificity in Turkey,
whereas pandemic impact expectedness positively influenced FBM
specificity in China and negatively in Denmark.
</td>
<td style="text-align:left;">
The flashbulb-like nature of memory for the first COVID-19 case and the
impact of the emergency. A cross-national survey.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Physical Activity, Heart Rate Variability, and Ventricular Arrhythmia
During the COVID-19 Lockdown: Retrospective Cohort Study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Investigating adaptive sport participation for adults aged 50 years or
older with spinal cord injury or disease: A descriptive cross-sectional
survey.
</td>
</tr>
<tr>
<td style="text-align:left;">
The Covid-19 pandemic has highlighted the precarity of urban society,
illustrating both opportunities and challenges. Teleworking rates
increased dramatically during the pandemic and may be sustained over the
long term. For transportation planners, these changes belie the broader
questions of how the geography of work and commuting will change based
on pandemic-induced shifts in teleworking and what this will mean for
society and policymaking. This study focuses on these questions by using
survey data (\<i\>n\</i\> = 2580) gathered in the autumn of 2021 to
explore the geography of current and prospective telework. The study
focuses on the Greater Golden Horseshoe, the mega-region in Southern
Ontario, representing a fifth of Canadians. Survey data document
telework practices before and during the pandemic, including prospective
future telework practices. Inferential models are used to develop
working-from-home scenarios which are allocated spatially based on
respondents’ locations of work and residence. Findings indicate that
telework appears to be poised to increase most relative to pre-pandemic
levels around downtown Toronto based on locations of work, but increases
in teleworking are more dispersed based on employees’ locations of
residence. Contrary to expectations by many, teleworking is not
significantly linked to home-work disconnect - suggesting that telework
is poised to weaken the commute-housing trade-off embedded in bid rent
theory. Together, these results portend a poor outlook for downtown
urban agglomeration economies but also more nuanced impacts than simply
inducing sprawl.
</td>
<td style="text-align:left;">
What might working from home mean for the geography of work and
commuting in the Greater Golden Horseshoe, Canada?
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
SARS-CoV-2 antibodies and their neutralizing capacity against live virus
in human milk after COVID-19 infection and vaccination: prospective
cohort studies.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Cannabis-involvement in emergency department visits for self-harm
following medical and non-medical cannabis legalization.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Evaluating a novel accelerated free-breathing late gadolinium
enhancement imaging sequence for assessment of myocardial injury.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Sexual and Reproductive Health Outcomes Among Adolescent Females During
the COVID-19 Pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Enhancing detection of SARS-CoV-2 re-infections using longitudinal
sero-monitoring: demonstration of a methodology in a cohort of people
experiencing homelessness in Toronto, Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Diagnostic performance of deep learning models versus radiologists in
COVID-19 pneumonia: A systematic review and meta-analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
The objective of this study was to investigate the association between
children’s parent-reported physical activity levels and weight changes
during the COVID-19 pandemic among children and youth in Ontario Canada.
A cross-sectional online survey was conducted in parents of children
5-17 years living in Ontario from May to July 2021. Parents recalled
their child’s physical activity and weight change during the year prior
to their completion of the survey. Odds ratios (OR) and 95% confidence
intervals (CI) were estimated using multinomial logistic regression for
the association between physical activity and weight gain or loss,
adjusted for child age and gender, parent ethnicity, current housing
type, method of school delivery, and financial stability. Overall, 86.8%
of children did not obtain 60 minutes of moderate-to-vigorous physical
activity per day and 75.4% of parents were somewhat or very concerned
about their child’s physical activity levels. For all physical activity
exposures (outdoor play, light physical activity, and
moderate-to-vigorous physical activity), lower physical activity was
consistently associated with increased odds of weight gain or loss. For
example, the adjusted OR for the association between 0-1 days of
moderate-to-vigorous physical activity versus 6-7 days and child weight
gain was 5.81 (95% CI 4.47, 7.56). Parent concern about their child’s
physical activity was also strongly associated with child weight gain
(OR 7.29; 95% CI 5.94, 8.94). No differences were observed between boys
and girls. This study concludes that a high proportion of children in
Ontario had low physical activity levels during the COVID-19 pandemic
and that low physical activity was strongly associated with parent
reports of both weight gain and loss among children.
</td>
<td style="text-align:left;">
Physical activity and unexpected weight change in Ontario children and
youth during the COVID-19 pandemic: A cross-sectional analysis of the
Ontario Parent Survey 2.
</td>
</tr>
<tr>
<td style="text-align:left;">
Nafamostat mesylate, a synthetic serine protease inhibitor, has
demonstrated early antiviral activity against SARS-CoV-2 and
anticoagulant properties that may be beneficial in COVID-19. We
conducted a meta-analysis evaluating the efficacy and safety of
nafamostat mesylate for COVID-19 treatment. PubMed, Embase, Cochrane
Library, Scopus, Web of Science, medRxiv and bioRxiv were searched up to
July 2023 for studies comparing outcomes between nafamostat mesylate
treatment and no nafamostat mesylate treatment in COVID-19 patients.
Mortality, disease progression and adverse events were analyzed. Six
studies involving 16,195 patients were included. Meta-analysis revealed
no significant difference in mortality (OR=0.88, 95%CI: 0.20-3.75,
P=0.86) or disease progression (OR=2.76, 95%CI: 0.31-24.68, P=0.36)
between groups. However, nafamostat mesylate was associated with
increased hyperkalemia risk (OR=7.15, 95%CI: 2.66 to 19.24,
P&lt;0.0001). Nafamostat mesylate does not improve mortality or
morbidity in hospitalized COVID-19 patients compared to no nafamostat
mesylate treatment. The significant hyperkalemia risk is a serious
concern requiring monitoring and preventative measures. Further research
is needed in different COVID-19 populations.
</td>
<td style="text-align:left;">
The Efficacy and Safety of Nafamostat Mesylate in the Treatment of
COVID-19: A Meta-Analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
The COVID-19 pandemic has forced governments across the world to
consider how to prioritise the allocation of scarce resources. There are
many tools and frameworks that have been designed to assist with the
challenges of priority setting in health care. The purpose of this study
was to examine the extent to which formal priority setting was evident
in the pandemic plans produced by countries in the World Health
Organisation’s EURO region, during the first wave of the COVID-19
pandemic. This compliments analysis of similar plans produced in other
regions of the world. Twenty four pandemic preparedness plans were
obtained that had been published between March and September 2020. For
data extraction, we applied a framework for identifying and assessing
the elements of good priority setting to each plan, before conducting
comparative analysis across the sample. Our findings suggest that while
some pre-requisites for effective priority setting were present in many
cases - including political commitment and a recognition of the need for
allocation decisions - many other hallmarks were less evident, such as
explicit ethical criteria, decision making frameworks, and engagement
processes. This study provides a unique insight into the role of
priority setting in the European response to the onset of the COVID-19
pandemic.
</td>
<td style="text-align:left;">
How did European countries set health priorities in response to the
COVID-19 threat? A comparative document analysis of 24 pandemic
preparedness plans across the EURO region.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Humoral Response Following 3 Doses of mRNA COVID-19 Vaccines in Patients
With Non-Dialysis-Dependent CKD: An Observational Study.
</td>
</tr>
<tr>
<td style="text-align:left;">
In the post COVID-19 pandemic period, targeted efforts are needed more
than ever to improve frontline nurses’ well-being. In the field of
palliative care, there is recognition of the importance of self-care,
but the concept itself remains nebulous, and proactive implementation of
self-care is lacking. Reflective writing has been noted to have positive
impacts on health care providers’ well-being. This piece brings to light
the author’s interest and work in reflective writing, sharing a personal
account that provides a source of happiness and an opportunity to better
understand her palliative care practice. Beyond the individual level,
organizations are also encouraged to invest in their nurses’ overall
well-being.
</td>
<td style="text-align:left;">
Promoting Self-Care in Palliative Care: Through the Wisdom of My
Grandmother.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
COVID-19 Reinfection Has Better Outcomes Than the First Infection in
Solid Organ Transplant Recipients.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Evaluating the Impact of Virtual Reality on the Behavioral and
Psychological Symptoms of Dementia and Quality of Life of Inpatients
With Dementia in Acute Care: Randomized Controlled Trial (VRCT).
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Effectiveness of the Wellness Together Canada Portal as a Digital Mental
Health Intervention in Canada: Protocol for a Randomized Controlled
Trial.
</td>
</tr>
<tr>
<td style="text-align:left;">
Healthcare workers are at risk of adverse mental health outcomes due to
occupational stress. Many organizations introduced initiatives to
proactively support staff’s psychological well-being in the face of the
COVID-19 pandemic. One example is the STEADY wellness program, which was
implemented in a large trauma centre in Toronto, Canada. Program
implementors engaged teams in peer support sessions, psychoeducation
workshops, critical incident stress debriefing, and community-building
initiatives. As part of a project designed to illuminate the experiences
of STEADY program implementors, this article describes recommendations
for future hospital wellness programs. Participants described the
importance of having the hospital and its leaders engage in supporting
staff’s psychological well-being. They recommended ways of doing so
(e.g., incorporating conversations about wellness in staff onboarding
and routine meetings), along with ways to increase program uptake and
sustainability (e.g., using technology to increase accessibility).
Results may be useful in future efforts to bolster hospital wellness
programming.
</td>
<td style="text-align:left;">
Recommendations for supporting healthcare workers’ psychological
well-being: Lessons learned during the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The clinical application of traditional Chinese medicine NRICM101 in
hospitalized patients with COVID-19.
</td>
</tr>
<tr>
<td style="text-align:left;">
We examined the perspectives of the Red River Métis citizens in
Manitoba, Canada, during the H1N1 and COVID-19 pandemics and how they
interpreted the communication of government/health authorities’ risk
management decisions. For Indigenous populations, pandemic response
strategies play out within the context of ongoing colonial relationships
with government institutions characterized by significant distrust. A
crucial difference between the two pandemics was that the Métis in
Manitoba were prioritized for early vaccine access during H1N1 but not
for COVID-19. Data collection involved 17 focus groups with Métis
citizens following the H1N1 outbreak and 17 focus groups during the
COVID-19 pandemic. Métis prioritization during H1N1 was met with some
apprehension and fear that Indigenous Peoples were vaccine-safety test
subjects before population-wide distribution occurred. By contrast, as
one of Canada’s three recognized Indigenous nations, the
non-prioritization of the Métis during COVID-19 was viewed as an
egregious sign of disrespect and indifference. Our research demonstrates
that both reactions were situated within claims that the government does
not care about the Métis, referencing past and ongoing colonial
motivations. Government and health institutions must anticipate this
overarching colonial context when making and communicating risk
management decisions with Indigenous Peoples. In this vein, government
authorities must work toward a praxis of decolonization in these
relationships, including, for example, working in partnership with
Indigenous nations to engage in collaborative risk mitigation and
communication that meets the unique needs of Indigenous populations and
limits the potential for less benign-though
understandable-interpretations.
</td>
<td style="text-align:left;">
“There’s a little bit of mistrust”: Red River Métis experiences of the
H1N1 and COVID-19 pandemics.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
A Novel Approach for the Early Detection of Medical Resource Demand
Surges During Health Care Emergencies: Infodemiology Study of Tweets.
</td>
</tr>
<tr>
<td style="text-align:left;">
Using data from 15 countries, this article investigates whether
descriptive and prescriptive gender norms concerning housework and child
care (domestic work) changed after the onset of the COVID-19 pandemic.
Results of a total of 8,343 participants (\<i\>M\</i\> = 19.95,
\<i\>SD\</i\> = 1.68) from two comparable student samples suggest that
descriptive norms about unpaid domestic work have been affected by the
pandemic, with individuals seeing mothers’ relative to fathers’ share of
housework and child care as even larger. Moderation analyses revealed
that the effect of the pandemic on descriptive norms about child care
decreased with countries’ increasing levels of gender equality;
countries with stronger gender inequality showed a larger difference
between pre- and post-pandemic. This study documents a shift in
descriptive norms and discusses implications for gender
equality-emphasizing the importance of addressing the additional
challenges that mothers face during health-related crises.
</td>
<td style="text-align:left;">
Did Descriptive and Prescriptive Norms About Gender Equality at Home
Change During the COVID-19 Pandemic? A Cross-National Investigation.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Acceptability of the Long-Term In-Home Ventilator Engagement virtual
intervention for home mechanical ventilation patients during the
COVID-19 pandemic: A qualitative evaluation.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Joint angle estimation during shoulder abduction exercise using
contactless technology.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
A comparison between different patient groups for diabetes management
during phases of the COVID-19 pandemic: a retrospective cohort study in
Ontario, Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
Youth with disabilities can benefit from social connections on virtual
platforms in terms of physical access to social spaces and opportunities
to communicate in alternative waysFor some youth with disabilities,
virtual social connections can be the only feasible and readily
available option for reducing social isolation due to physical barriers
to accessWhen offering virtual program options, service providers should
consider the various benefits of connecting with the physical,
communication-based, interaction-based, access-based and other barriers
to virtual connection.
</td>
<td style="text-align:left;">
Facilitating virtual social connections for youth with disabilities:
lessons for post-COVID-19 programming.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Determinants of SARS-CoV-2 IgG response and decay in Canadian healthcare
workers: A prospective cohort study.
</td>
</tr>
<tr>
<td style="text-align:left;">
Our understanding of the quality of cellular and humoral immunity
conferred by COVID-19 vaccination alone versus vaccination plus
SARS-CoV-2 breakthrough (BT) infection remains incomplete. While the
current (2023) SARS-CoV-2 immune landscape of Canadians is complex, in
late 2021 most Canadians had either just received a third dose of
COVID-19 vaccine, or had received their two-dose primary series and then
experienced an Omicron BT. Herein we took advantage of this coincident
timing to contrast cellular and humoral immunity conferred by three
doses of vaccine versus two doses plus BT. Our results show thatBT
infection induces cell-mediated immune responses to variants comparable
to an intramuscular vaccine booster dose. In contrast, BT subjects had
higher salivary immunoglobulin (Ig)G and IgA levels against the Omicron
spike and enhanced reactivity to the ancestral spike for the IgA
isotype, which also reacted with SARS-CoV-1. Serumneutralizing antibody
levels against the ancestral strain and the variants were also higher
after BT infection. Our results support the need for the development of
intranasal vaccines that could emulate the enhanced mucosal and humoral
immunity induced by Omicron BT without exposing individuals to the risks
associated with SARS-CoV-2 infection.
</td>
<td style="text-align:left;">
Comparison of Omicron breakthrough infection versus monovalent
SARS-CoV-2 intramuscular booster reveals differences in mucosal and
systemic humoral immunity.
</td>
</tr>
<tr>
<td style="text-align:left;">
Supporting schooling for current and past pediatric oncology patients is
vital to their quality of life and psychosocial recovery. However, no
study has examined the perspectives toward in-person schooling among
pediatric oncology families during the COVID-19 pandemic. In this online
survey study, we determined the rate of and attitudes toward in-person
school attendance among current and past pediatric oncology patients
living in Ontario, Canada during the 2020-2021 school year. Of our
31-family cohort, 23 children (74%) did attend and 8 (26%) did not
attend any in-person school during this time. Fewer children within 2
years of treatment completion attended in-person school (5/8; 62%) than
those more than 2 years from treatment completion (13/15; 87%). Notably,
22 of 29 parents (76%) felt that speaking to their care team had the
greatest impact compared to other potential information sources when
deciding about school participation, yet 13 (45%) were unaware of their
physician’s specific recommendation regarding whether their child should
attend. This study highlights the range in parental comfort regarding
permitting in-person schooling during the COVID-19 pandemic. Pediatric
oncologists should continue to address parental concerns around
in-person school during times of high transmission of COVID-19 and
potentially other communicable diseases in the future.
</td>
<td style="text-align:left;">
School Attendance Among Pediatric Oncology Patients During the COVID-19
Pandemic in Ontario, Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Planning with a gender lens: A gender analysis of pandemic preparedness
plans from eight countries in Africa.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The impact of COVID-19 on nurses’ job satisfaction: a systematic review
and meta-analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Establishing association between HLA-C\*04:01 and severe COVID-19.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Strategies to support maternal and early childhood wellness: insight
from parent and provider qualitative interviews during the COVID-19
pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Evaluating in vivo effectiveness of sotrovimab for the treatment of
Omicron subvariant BA.2 versus BA.1: a multicentre, retrospective cohort
study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Virtual Cancer Care Beyond the COVID-19 Pandemic: Patient and Staff
Perspectives and Recommendations.
</td>
</tr>
<tr>
<td style="text-align:left;">
The COVID-19 pandemic has disrupted the lives of people with
intellectual disabilities in many ways, impacting their health and
wellbeing. Early in the pandemic, the research team delivered a six-week
virtual group-based program to help Canadian adults with intellectual
disabilities cope and better manage their mental health. The study’s
objective was to explore ongoing concerns among individuals with
intellectual disabilities following their participation in this
education and support program. Thematic analysis was used to analyze
participant feedback provided eight weeks after course completion.
Twenty-four participants were interviewed in January 2021 and May 2021
across two cycles of the course. Three themes emerged: 1) employment and
financial challenges; 2) navigating changes and ongoing restrictions;
and 3) vaccine anticipation and experience. These findings suggest that
despite benefiting from the program, participants continued to
experience pandemic-related challenges in 2021, emphasising the need to
continually engage people with intellectual disabilities.
</td>
<td style="text-align:left;">
“Everything has changed since COVID”: Ongoing challenges faced by
Canadian adults with intellectual disabilities during waves 2 and 3 of
the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of the COVID-19 pandemic on perceived changes in responsibilities
for adult caregivers who support children and youth in Ontario, Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
Early in the COVID-19 pandemic, multiple event stressors converged to
exacerbate a growing mental health crisis in Canada with differing
effects across status groups. However, less is known about changing
mental health situations throughout the pandemic, especially among
individuals more likely to experience chronic stress because of their
disability and health status. Using data from two waves of a targeted
online survey of people with disabilities and chronic health conditions
in Canada (N = 563 individuals, June 2020 and July 2021), we find that
approximately 25% of respondents experienced additional increases in
stress and anxiety levels in 2021. These increases were partly explained
by worsening perceived financial insecurity and, in the case of stress,
additional negative financial effects tied to the pandemic. This paper
understands mental health disparities as a function of social status and
social group membership. By linking stress process models and a minority
stress framework with a social model of disability, we allude to how
structural and contextual barriers make functional limitations disabling
and in turn, life stressors.
</td>
<td style="text-align:left;">
Economic precarity and changing levels of anxiety and stress among
Canadians with disabilities and chronic health conditions throughout the
COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
The global spread of severe acute respiratory syndrome coronavirus 2019
(COVID-19) has affected over 100 countries and has led to the tragic
loss of life, overwhelmed health care systems and severely impacted the
global economy. Specifically, individuals living with spinal cord injury
(SCI) are particularly vulnerable during the COVID-19 pandemic as they
often face adverse impacts on their health, emotional well-being,
community participation, and life expectancy. The objective of this
study was to investigate the lived experience of individuals with SCI
during the COVID-19 pandemic in Ontario, Canada. An exploratory design
with a qualitative descriptive approach was used to address the study
objective. Nine semi-structured interviews were conducted with
individuals with traumatic and non-traumatic SCI (37-69 years, C3-L5,
AIS A-D, and 5-42 years post-injury). Using reflexive thematic analysis,
the following themes were created: (1) Caregiver exposure to COVID-19;
(2) Staying physically active in quarantine; (3) Living in social
isolation; (4) Difficulty obtaining necessary medical supplies; (5)
Access to health services and virtual care during COVID-19; and (6)
Fighting COVID-19 misinformation. This is one of the first studies to
explore the impact of COVID-19 on individuals living with SCI in
Ontario. This study contributes to a greater understanding of the
challenges faced by individuals living with SCI and provides insight
into how to better support and respond to the specific and unique needs
of individuals with SCI and their families during a national emergency
or pandemic.
</td>
<td style="text-align:left;">
Exploring the Lived Experiences of Individuals With Spinal Cord Injury
During the COVID-19 Pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
Severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2),
responsible for the COVID-19 pandemic, uses a surface expressed trimeric
spike glycoprotein for cell entry. This trimer is the primary target for
neutralizing antibodies making it a key candidate for vaccine
development. During the global pandemic circulating variants of concern
(VOC) caused several waves of infection, severe disease, and death. The
reduced efficacy of the ancestral trimer-based vaccines against emerging
VOC led to the need for booster vaccines. Here we present a detailed
characterization of the Sanofi Beta trimer, utilizing cryo-EM for
structural elucidation. We investigate the conformational dynamics and
stabilizing features using orthogonal SPR, SEC, nanoDSF, and HDX-MS
techniques to better understand how this antigen elicits superior broad
neutralizing antibodies as a variant booster vaccine. This structural
analysis confirms the Beta trimer preference for canonical quaternary
structure with two RBD in the up position and the reversible equilibrium
between the canonical spike and open trimer conformations. Moreover,
this report provides a better understanding of structural differences
between spike antigens contributing to differential vaccine efficacy.
</td>
<td style="text-align:left;">
Structural and biochemical rationale for Beta variant protein booster
vaccine broad cross-neutralization of SARS-CoV-2.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Motivation to participate and attrition factors in a COVID-19 biobank: A
qualitative study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Effectiveness of a fourth mRNA dose among individuals with systemic
autoimmune rheumatic diseases during the Omicron era.
</td>
</tr>
<tr>
<td style="text-align:left;">
Radiotherapy (RT) has a fundamental role in the treatment of gynecologic
malignancies, including cervical and uterine cancers. Hypofractionated
RT has gained popularity in many cancer sites, boosted by technological
advances in treatment delivery and image verification. Hypofractionated
RT uptake was intensified during the COVID-19 pandemic and has the
potential to improve universal access to radiotherapy worldwide,
especially in low-resource settings. This review summarizes the
rationale, the current challenges and investigation efforts, together
with the recent developments associated with hypofractionated RT in
gynecologic malignancies. A comprehensive search was undertaken using
multiple databases and ongoing trial registries. In the definitive
radiotherapy setting for cervical cancers, there are several ongoing
clinical trials from Canada, Mexico, Iran, the Philippines and Thailand
investigating the role of a moderate hypofractionated external beam RT
regimen in the low-risk locally advanced population. Likewise, there are
ongoing ultra and moderate hypofractionated RT trials in the uterine
cancer setting. One Canadian prospective trial of stereotactic
hypofractionated adjuvant RT for uterine cancer patients suggested a
good tolerance to this treatment strategy in the acute setting, with a
follow-up trial currently randomizing patients between conventional
fractionation and the hypofractionated dose regimen delivered in the
former trial. Although not yet ready for prime-time use,
hypofractionated RT could be a potential solution to several challenges
that limit access to and the utilization of radiotherapy for gynecologic
cancer patients worldwide.
</td>
<td style="text-align:left;">
Hypofractionated Radiotherapy in Gynecologic Malignancies-A Peek into
the Upcoming Evidence.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Disproportionate Rates of COVID-19 Among Black Canadian Communities:
Lessons from a Cross-Sectional Study in the First Year of the Pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Effects of a Rice-Farming Simulation Video Game on Nature Relatedness,
Nutritional Status, and Psychological State in Urban-Dwelling Adults
During the COVID-19 Pandemic: Randomized Waitlist Controlled Trial.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The effect of dupilumab on clinical outcomes in patients with COVID-19:
A meta-analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
The initial two-dose vaccine series and subsequent booster vaccine doses
have been effective in modulating SARS-CoV-2 disease severity and death
but do not completely prevent infection. The correlates of infection
despite vaccination continue to be under investigation. In this
prospective decentralized study (n = 1286) comparing antibody responses
in an older- (≥70 years) to a younger-aged cohort (aged 30-50 years), we
explored the correlates of breakthrough infection in 983 eligible
subjects. Participants self-reported data on initial vaccine series,
subsequent booster doses and COVID-19 infections in an online portal and
provided self-collected dried blood spots for antibody testing by ELISA.
Multivariable survival analysis explored the correlates of breakthrough
infection. An association between higher antibody levels and protection
from breakthrough infection observed during the Delta and Omicron BA.1/2
waves of infection no longer existed during the Omicron BA.4/5 wave. The
older-aged cohort was less likely to have a breakthrough infection at
all time-points. Receipt of an original/Omicron vaccine and the presence
of hybrid immunity were associated with protection of infection during
the later Omicron BA.4/5 and XBB waves. We were unable to determine a
threshold antibody to define protection from infection or to guide
vaccine booster schedules.
</td>
<td style="text-align:left;">
Predictors of Breakthrough SARS-CoV-2 Infection after Vaccination.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Predictors of later COVID-19 test seeking.
</td>
</tr>
<tr>
<td style="text-align:left;">
Although COVID-19-related physical distancing has had large economic
consequences, the impact on volunteerism is unclear. Using volunteer
position postings data from Canada’s largest volunteer center (Volunteer
Toronto) from February 3, 2020, to January 4, 2021, we evaluated the
impact of different levels of physical distancing on average views,
total views, and total number of posts. There was about a 50% decrease
in the total number of posts that was sustained throughout the pandemic.
Although a more restrictive physical distancing policy was generally
associated with fewer views, there was an initial increase in views
during the first lockdown where total views were elevated for the first
4 months of the pandemic. This was driven by interest in
COVID-19-related and remote work postings. This highlights the community
of volunteers may be quite flexible in terms of adapting to new ways of
volunteering, but substantial challenges remain for the continued
operations of many non-profit organizations.
</td>
<td style="text-align:left;">
COVID-19 Pandemic, Physical Distancing Policies, and the Non-Profit
Sector Volunteer Force.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Effect of COVID-19 on the prevalence of bystanders performing
cardiopulmonary resuscitation: A systematic review and meta-analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
To correlate pain-related phenotyping for central nervous system
sensitization in endometriosis-associated pain with mental health
outcomes during the COVID-19 pandemic, the prospective Endometriosis and
Pelvic Pain Interdisciplinary Cohort (ClinicalTrials.gov \#NCT02911090)
was linked to the COVID-19 Rapid Evidence Study of a Provincial
Population-Based Cohort for Gender and Sex (RESPPONSE) dataset. The
primary outcomes were depression (PHQ-9) and anxiety (GAD-7) scores
during the pandemic. The explanatory variables of interest were the
Central Sensitization Inventory (CSI) score (0-100) and
endometriosis-associated chronic pain comorbidities/psychological
variables before the pandemic. The explanatory and response variables
were assessed for correlation, followed by multivariable regression
analyses adjusting for PHQ-9 and GAD-7 scores pre-pandemic as well as
age, body mass index, and parity. A higher CSI score and a greater
number of chronic pain comorbidities before the pandemic were both
positively correlated with PHQ-9 and GAD-7 scores during the pandemic.
These associations remained significant in adjusted analyses. Increasing
the CSI score by 10 was associated with an increase in pandemic PHQ-9 by
.74 points (P &lt; .0001) and GAD-7 by .73 points (P &lt; .0001) on
average. Each additional chronic pain comorbidity/psychological variable
was associated with an increase in pandemic PHQ-9 by an average of .63
points (P = .0004) and GAD-7 by .53 points (P = .0002). Endometriosis
patients with a history of central sensitization before the pandemic had
worse mental health outcomes during the COVID-19 pandemic. As a risk
factor for mental health symptoms in the face of major stressors,
clinical proxies for central sensitization can be used to identify
endometriosis patients who may need additional support. PERSPECTIVE:
This article adds to the growing literature of the clinical importance
of central sensitization in endometriosis patients, who had more
symptoms of depression and anxiety during the COVID-19 pandemic.
Clinical features of central sensitization may help clinicians identify
endometriosis patients needing additional support when facing major
stressors.
</td>
<td style="text-align:left;">
Mental Health Outcomes of Endometriosis Patients during the COVID-19
Pandemic: Impact of Pre-pandemic Central Nervous System Sensitization.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Geriatric care physicians’ perspectives on providing virtual care: a
reflexive thematic synthesis of their online survey responses from
Ontario, Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Substance use care innovations during COVID-19: barriers and
facilitators to the provision of safer supply at a toronto COVID-19
isolation and recovery site.
</td>
</tr>
<tr>
<td style="text-align:left;">
Under what conditions do citizens support coercive public policies?
Although recent research suggests that people prefer policies that
preserve freedom of choice, such as behavioural nudges, many citizens
accepted stringent policy interventions like fines and mandates to
promote vaccination during the COVID-19 pandemic-a pattern that may be
linked to the unusually high effectiveness of COVID-19 vaccines. We
conducted a large online survey experiment (N = 42,417) in the Group of
Seven (G-7) countries investigating the relationship between a policy’s
effectiveness and public support for stringent policies. Our results
indicate that public support for stringent vaccination policies
increases as vaccine effectiveness increases, but at a modest scale.
This relationship flattens at higher levels of vaccine effectiveness.
These results suggest that intervention effectiveness can be a
significant predictor of support for coercive policies but only up to
some threshold of effectiveness.
</td>
<td style="text-align:left;">
Public support for more stringent vaccine policies increases with
vaccine effectiveness.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Child abuse and neglect during the COVID-19 pandemic: An umbrella
review.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
COVID-19 Pivoted Virtual Skills Teaching Model: Project ECHO Ontario
Skin and Wound Care Boot Camp.
</td>
</tr>
<tr>
<td style="text-align:left;">
The Alzheimer Society of Ontario’s Minds in Motion (MiM) program
improves physical function and well-being of people living with dementia
(PLWD) and their care partners (CP) (Regan et al., 2019). With the
COVID-19 pandemic, there was an urgent need to transition to a virtual
MiM that was similarly safe and effective. The purpose of this mixed
methods study is to describe the standardized, virtual MiM and evaluate
its acceptability, and impact on quality of life, and physical and
cognitive activity of participants. Survey of ad hoc virtual MiM
practices and a literature review informed the design of the
standardized MiM program: 8 weeks of weekly 90-minute sessions that
included 45-minutes of physical activity and 45-minutes of cognitive
stimulation in each session. Participants completed a standardized,
virtual MiM at one of 6 participating Alzheimer Societies in Ontario, as
well as assessments of quality of life, physical and cognitive activity,
and program satisfaction pre- and post-program. In all, 111 PLWD and 90
CP participated in the evaluation (average age of 74.6±9.4 years, 61.2%
had a college/university degree or greater, 80.6% were married, 48.6% of
PLWD and 75.6% of CP were women). No adverse events occurred. MiM
participants rated the program highly (average score of 4.5/5). PLWD
reported improved quality of life post-MiM (p = &lt;0.01). Altogether,
participants reported increased physical activity levels (p = &lt;0.01)
and cognitive activity levels (p = &lt;0.01). The virtual MiM program is
acceptable, safe, and effective at improving quality of life, cognitive
and physical activity levels for PLWD, and cognitive and physical
activity levels among CP.
</td>
<td style="text-align:left;">
Enhancing Minds in Motion® as a virtual program delivery model for
people living with dementia and their care partners.
</td>
</tr>
<tr>
<td style="text-align:left;">
\<b\>Contexte:\</b\> Le syndrome de la douleur post-virale, également
connu sous le nom de syndrome post-viral, est une affection complexe
caractérisée par des douleurs persistantes, de la fatigue, des douleurs
musculosquelettiques, des douleurs neuropathiques, des difficultés
neurocognitives et des troubles du sommeil qui peuvent survenir après la
guérison d’une infection virale.\<b\>Objectifs:\</b\> Cette revue
narrative présente un résumé des séquelles des syndromes post-viraux,
des agents viraux qui les causent, ainsi que de la pathophysiologie, des
traitements et des considérations futures pour la recherche et les
traitements ciblés.\<b\>Méthodes utilisées:\</b\> Les bases de données
Medline, PubMed et Embase ont été utilisées pour rechercher des études
sur les virus associés au syndrome post-viral.\<b\>Conclusion:\</b\> La
physiopathologie des syndromes post-viraux reste largement méconnue et
peu d’études ont présenté un résumé complet de l’affection, des agents
qui la provoquent et des modalités de traitement efficaces. Alors que la
pandémie de COVID-19 continue d’affecter des millions de personnes dans
le monde, il est primordial de comprendre l’étiologie de la maladie
post-virale et de savoir comment aider les individus à faire face aux
séquelles.
</td>
<td style="text-align:left;">
Post-Viral Pain, Fatigue, and Sleep Disturbance Syndromes: Current
Knowledge and Future Directions.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Understanding attitudes and beliefs regarding COVID-19 vaccines among
transitional-aged youth with mental health concerns: a youth-led
qualitative study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Evaluating the impact of a SIMPlified LaYered consent process on
recruitment of potential participants to the \<i\>Staphylococcus
aureus\</i\> Network Adaptive Platform trial: study protocol for a
multicentre pragmatic nested randomised clinical trial (SIMPLY-SNAP
trial).
</td>
</tr>
<tr>
<td style="text-align:left;">
Bilevel-positive airway pressure (BiPAP) is a noninvasive respiratory
support modality which reduces effort in patients with respiratory
failure. However, it may increase tidal ventilation and transpulmonary
pressure, potentially aggravating lung injury. We aimed to assess if the
use of BiPAP before intubation was associated with increased mortality
in adult patients with coronavirus disease 2019 (COVID-19) who received
venovenous extracorporeal membrane oxygenation (ECMO). We used the
Extracorporeal Life Support Organization Registry to analyze adult
patients with COVID-19 supported with venovenous ECMO from January 1,
2020, to December 31, 2021. Patients treated with BiPAP were compared
with patients who received other modalities of respiratory support or no
respiratory support. A total of 9,819 patients from 421 centers were
included. A total of 3,882 of them (39.5%) were treated with BiPAP
before endotracheal intubation. Patients supported with BiPAP were
intubated later (4.3 vs. 3.3 days, p &lt; 0.001) and showed higher
unadjusted hospital mortality (51.7% vs. 44.9%, p &lt; 0.001). The use
of BiPAP before intubation and time from hospital admission to
intubation resulted as independently associated with increased hospital
mortality (odds ratio \[OR\], 1.32 \[95% confidence interval {CI},
1.08-1.61\] and 1.03 \[1-1.06\] per day increase). In ECMO patients with
severe acute respiratory failure due to COVID-19, the extended use of
BiPAP before intubation should be regarded as a risk factor for
mortality.
</td>
<td style="text-align:left;">
Noninvasive Ventilation Before Intubation and Mortality in Patients
Receiving Extracorporeal Membrane Oxygenation for COVID-19: An Analysis
of the Extracorporeal Life Support Organization Registry.
</td>
</tr>
<tr>
<td style="text-align:left;">
The 2022 FIFA World Cup was the first major multi-continental sporting
Mass Gathering Event (MGE) of the post COVID-19 era to allow foreign
spectators. Such large-scale MGEs can potentially lead to outbreaks of
infectious disease and contribute to the global dissemination of such
pathogens. Here we adapt previous work and create a generalisable model
framework for assessing the use of disease control strategies at such
events, in terms of reducing infections and hospitalisations. This
framework utilises a combination of meta-populations based on clusters
of people and their vaccination status, Ordinary Differential Equation
integration between fixed time events, and Latin Hypercube sampling. We
use the FIFA 2022 World Cup as a case study for this framework
(modelling each match as independent 7 day MGEs). Pre-travel screenings
of visitors were found to have little effect in reducing COVID-19
infections and hospitalisations. With pre-match screenings of spectators
and match staff being more effective. Rapid Antigen (RA) screenings 0.5
days before match day performed similarly to RT-PCR screenings 1.5 days
before match day. Combinations of pre-travel and pre-match testing led
to improvements. However, a policy of ensuring that all visitors had a
COVID-19 vaccination (second or booster dose) within a few months before
departure proved to be much more efficacious. The State of Qatar
abandoned all COVID-19 related travel testing and vaccination
requirements over the period of the World Cup. Our work suggests that
the State of Qatar may have been correct in abandoning the pre-travel
testing of visitors. However, there was a spike in COVID-19 cases and
hospitalisations within Qatar over the World Cup. Given our findings and
the spike in cases, we suggest a policy requiring visitors to have had a
recent COVID-19 vaccination should have been in place to reduce cases
and hospitalisations.
</td>
<td style="text-align:left;">
Modelling disease mitigation at mass gatherings: A case study of
COVID-19 at the 2022 FIFA World Cup.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Pivoting school health and nutrition programmes during COVID-19 in low-
and middle-income countries: A scoping review.
</td>
</tr>
<tr>
<td style="text-align:left;">
COVID-19 unleashed a maelstrom of distress on health care professionals.
The pandemic contributed to a host of stressors for workers because of
the need for rapid acquisition of new knowledge and skills to provide
best treatment while simultaneously dealing with personal safety,
limited resources, staffing shortages, and access to care issues.
Concurrently, problems with systemic racial inequality and
discrimination became more apparent secondary to difficulties with
accessing health care for minorities and other marginalized groups.
These problems contributed to many health care professionals
experiencing severe moral injury and burnout as they struggled to uphold
core values and do their jobs professionally. Some left or disengaged.
Others died. As continuing professional development leaders focused on
all health professionals, we must act deliberately to address health
care professionals’ distress and mental health. We must incorporate
wellness and mental health as organizing principles in all we do. We
must adopt a new mental model that recognizes the importance of
learners’ biopsychosocial functioning and commit to learners’ wellness
by developing activities that embrace a biopsychosocial point of view.
As educators and influencers, we must demonstrate that the Institute for
Healthcare Improvement’s fourth aim to improve clinician well-being and
safety (2014) and fifth aim to address health equity and the social
determinants of health (2021) matter. It is crucial that continuing
professional development leaders globally use their resources and
relationships to accomplish this imperative call for action.
</td>
<td style="text-align:left;">
Health Care Professional Distress and Mental Health: A Call to the
Continuing Professional Development Community.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of the COVID-19 pandemic on the adaptability and resiliency of
school food programs across Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
The COVID-19 pandemic has highlighted the importance of global health
diplomacy (GHD), with India emerging as a key player. India’s commitment
to GHD is demonstrated by its active participation in regional and
multilateral projects, pharmaceutical expertise, and large-scale
manufacturing capabilities, which include the production and
distribution of COVID-19 vaccines and essential medicines. India has
supported nations in need through bilateral and multilateral platforms,
providing vaccines to countries experiencing shortages and offering
technical assistance and capacity-building programs to improve
healthcare infrastructure and response capabilities. India’s unique
approach to GHD, rooted in humanitarian diplomacy, emphasized
collaboration and empathy and stressed the well-being of humanity by
embracing the philosophy of “Vasudhaiva Kutumbakam,” which translates to
“\<i\>the world is one family\</i\>.” Against this background, this
paper’s main focus is to analyze the rise of India’s GHD amidst the
COVID-19 pandemic and its leadership in addressing various global
challenges. India has demonstrated its commitment to global solidarity
by offering medical supplies, equipment, and expertise to more than 100
countries. India’s rising global leadership can be attributed to its
proactive approach, humanitarian diplomacy, and significant
contributions to global health initiatives.
</td>
<td style="text-align:left;">
The rise of India’s global health diplomacy amid COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
We conducted a tabletop exercise on influenza outbreak preparedness that
engaged a large group of congregate living settings (CLS), with
improvements in self-reported knowledge and readiness. This proactive
approach to responding to communicable disease threats has potential to
build infection prevention and control capacity beyond COVID-19 in the
CLS sector.
</td>
<td style="text-align:left;">
Influenza outbreak management tabletop exercise for congregate living
settings.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Protocol and statistical analysis plan for the identification and
treatment of hypoxemic respiratory failure and acute respiratory
distress syndrome with protection, paralysis, and proning: A type-1
hybrid stepped-wedge cluster randomised effectiveness-implementation
study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Advancing language concordant care: a multimodal medical interpretation
intervention.
</td>
</tr>
<tr>
<td style="text-align:left;">
RéSUMé: OBJECTIFS: Explorer les expériences de femmes métisses et de
personnes métisses bispirituelles et de diverses identités de genre
ayant accédé aux services sociaux et de santé à Victoria
(Colombie-Britannique) pendant la pandémie de COVID-19, et en tirer des
leçons. MéTHODE: Cet article vient d’une vaste étude sur les expériences
de femmes métisses et de personnes métisses bispirituelles et de
diverses identités de genre ayant accédé aux services sociaux et de
santé à Victoria. À l’aide d’une démarche par et pour les personnes
métisses qui a fait appel à une méthode d’entrevue directe, nous avons
mené des entrevues avec des femmes métisses et des personnes
bispirituelles et de diverses identités de genre ayant vécu à Victoria
en décembre 2020 et janvier 2021 et/ou accédé à des services dans cette
ville durant cette période. Le présent article porte spécifiquement sur
les données liées aux incidences de la COVID-19 chez ces personnes.
RéSULTATS: En tout, 24 femmes et personnes métisses bispirituelles et de
diverses identités de genre ont participé à l’étude. Dans l’ensemble,
trois aspects relatifs à la COVID-19 sont ressortis des données.
Premièrement, les personnes participantes ont décrit les effets
préjudiciables de la COVID-19 sur leur capacité de rester en lien avec
leur communauté métisse et de pratiquer leur culture, ainsi que leurs
sentiments d’isolement en général. Deuxièmement, elles ont souligné
certaines des façons dont la COVID-19 a exacerbé les barrières
existantes à l’accès aux soins de santé culturellement sûrs. Enfin, les
personnes participantes ont parlé des retombées économiques mitigées de
la COVID-19 dans leur cas, et elles ont partagé leurs idées sur le rôle
du genre, en particulier, dans leur instabilité financière. CONCLUSION:
Pour atténuer les effets préjudiciables disproportionnés de la pandémie
et améliorer les résultats cliniques globaux au sein des communautés
métisses du Canada, il est essentiel d’améliorer l’accès aux services
sociaux et de santé culturellement sûrs en y intégrant les expériences
et le savoir-faire de femmes métisses et de personnes métisses
bispirituelles et de diverses identités de genre.
</td>
<td style="text-align:left;">
“I’m still not over feeling so isolated”: Métis women, Two-Spirit, and
gender-diverse people’s experiences of the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Biopsychosocial risk factors for intimate partner violence perpetration
and victimization in adolescents and adults reported after the COVID-19
pandemic onset: a scoping review protocol.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Acute health care use among children during the first 2.5 years of the
COVID-19 pandemic in Ontario, Canada: a population-based repeated
cross-sectional study.
</td>
</tr>
<tr>
<td style="text-align:left;">
Systematic priority setting (PS), based on explicit criteria, is thought
to improve the quality and consistency of the PS decisions. Among the PS
criteria, there is increased focus on the importance of equity
considerations and vulnerable populations. This paper discusses the PS
criteria that were included in the national COVID-19 pandemic plans,
with specific focus on equity and on the vulnerable populations
considered. Secondary synthesis of data, from a global comparative study
that examined the degree to which the COVID-19 plans included PS, was
conducted. Only 32 % of the plans identified explicit criteria. Severity
of the disease and/or disease burden were the commonly mentioned
criteria. With regards to equity considerations and prioritizing
vulnerable populations, 22 countries identified people with
co-morbidities others mentioned children, women etc. Low social-economic
status and internally displaced population were not identified in any of
the reviewed national plans. The limited inclusion of explicit criteria
and equity considerations highlight a need for policy makers, in all
contexts, to consider instituting and equipping PS institutions who can
engage diverse stakeholders in identifying the relevant PS criteria
during the post pandemic period. While vulnerability will vary with the
type of health emergency- awareness of this and having mechanisms for
identifying and prioritizing the most vulnerable will support equitable
pandemic responses.
</td>
<td style="text-align:left;">
A global comparative analysis of the criteria and equity considerations
included in eighty-six national COVID-19 plans.
</td>
</tr>
<tr>
<td style="text-align:left;">
RéSUMé: OBJECTIFS: Étudier l’évolution du risque d’infection et de
problèmes de santé mentale (PSM) chez les travailleurs de la santé
(TdS), comparé à la population générale, au cours de la pandémie de
COVID-19. MéTHODES: Certains TdS de l’Alberta (Canada) participant à une
cohorte interprovinciale, ont consenti à ce que la base administrative
de santé de l’Alberta (AHDB) nous transmette leurs données de
vaccination contre la COVID-19 et de tests d’amplification des acides
nucléiques (TAAN). Ceux ayant consenti ont été appariés à un maximum de
cinq témoins de population générale. Les diagnostics médicaux (par
médecins) de COVID-19 ont été identifiés dans l’AHDB du début de la
pandémie jusqu’au 31 mars 2022. Les consultations médicales pour PSM
(anxiété, stress/troubles de l’adaptation, dépression) ont été
identifiées entre le 1er avril 2017 et le 31 mars 2022. Les rapports de
cotes (RC) comparant les TdS aux témoins de la population générale ont
été estimés pour chaque vague d’infection. RéSULTATS: Quatre-vingts
pourcent (80 %; 3050/3812) des TdS ont donné leur consentement à ce que
leurs données nous soient transmises par l’AHDB; 97 % d’entre eux
(2959/3050) ont été appariés à 14 546 témoins. Dans l’ensemble, les TdS
étaient plus à risque de COVID-19, avec une première infection
identifiée soit par les TANN (RC=1,96, IC de 95% 1,76-2,17), soit via
les dossiers médicaux (RC=1,33, IC de 95% 1,21-1,45). Ils étaient
également plus à risque pour chacun des trois problèmes de SM. Le risque
de COVID-19 ajustés pour les facteurs de confusion était plus élevé que
chez les témoins au début de la pandémie et durant la cinquième vague
(variant Omicron). Les excès de risque de stress/troubles de
l’adaptation (RC=1,52, IC de 95% 1,35-1,71) et de dépression (RC=1,39,
IC de 95% 1,24-1,55) ont augmenté au fil des vagues de l’épidémie, avec
un pic à la quatrième vague. CONCLUSION: Les TdS étaient plus à risque
d’infection de COVID-19 et de troubles de santé mentale avec cet excès
de risque se prolongeant plus tard dans la pandémie.
</td>
<td style="text-align:left;">
Excess risk of COVID-19 infection and mental distress in healthcare
workers during successive pandemic waves: Analysis of matched cohorts of
healthcare workers and community referents in Alberta, Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Protection, freedom, stigma: a critical discourse analysis of face masks
in the first wave of the COVID-19 pandemic and implications for medical
education.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Investigating the experiences of medical students quarantined due to
COVID-19 exposure.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Factors associated with SARS-CoV-2 infection in unvaccinated children
and young adults.
</td>
</tr>
<tr>
<td style="text-align:left;">
Dissemination of accurate scientific and medical information was
critical during the coronavirus disease 2019 (COVID-19) pandemic,
particularly during the early phases when little was known about the
disease. Unfortunately, poor science literacy is a serious public health
problem, contributing to widespread difficulties in accurately
interpreting information and scientific data during the ongoing COVID-19
infodemic.\<sup\>1,2\</sup\>.
</td>
<td style="text-align:left;">
Role of Creation of Plain Language Summaries to Disseminate COVID-19
Research Findings to Patients With Rheumatic Diseases.
</td>
</tr>
<tr>
<td style="text-align:left;">
Strategies to ramp up breast cancer screening after COVID-19 require
data on the influence of the pandemic on groups of women with
historically low screening uptake. Using data from Ontario, Canada, our
objectives were to 1) quantify the overall pandemic impact on weekly
bilateral screening mammography rates (per 100,000) of average-risk
women aged 50-74 and 2) examine if COVID-19 has shifted any mammography
inequalities according to age, immigration status, rurality, and access
to material resources. Using a segmented negative binomial regression
model, we estimated the mean change in rate at the start of the pandemic
(the week of March 15, 2020) and changes in weekly trend of rates during
the pandemic period (March 15-December 26, 2020) compared to the
pre-pandemic period (January 3, 2016-March 14, 2020) for all women and
for each subgroup. A 3-way interaction term (COVID-19*week*subgroup
variable) was added to the model to detect any pandemic impact on
screening disparities. Of the 3,481,283 mammograms, 8.6 % (n = 300,064)
occurred during the pandemic period. Overall, the mean weekly rate
dropped by 93.4 % (95 % CI 91.7 % - 94.8 %) at the beginning of
COVID-19, followed by a weekly increase of 8.4 % (95 % CI 7.4 % - 9.4 %)
until December 26, 2020. The pandemic did not shift any disparities (all
interactions p &gt; 0.05) and that women who were under 60 or over 70,
immigrants, or with a limited access to material resources had
persistently low screening rate in both periods. Interventions should
proactively target these underserved populations with the goals of
reducing advanced-stage breast cancer presentations and mortality.
</td>
<td style="text-align:left;">
COVID-19 pandemic impact on the potential exacerbation of screening
mammography disparities: A population-based study in Ontario, Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
The advent of the COVID-19 pandemic and the establishment of a new
transitional government in Sudan with rejuvenated relations with the
international community paved the way for external assistance to the EU
COVID-19 response project, a project with a pioneering design within the
region. The project sought to operationalize the
humanitarian-development-peace nexus, perceiving the nexus as a
continuum rather than sequential due to the protracted nature of
emergencies in Sudan and their multiplicity and contextual complexity.
It went further into enhancing peace through engaging with conflict and
post-conflict-affected states and communities and empowering local
actors. Learning from this experience, external assistance models to
low- or middle-income countries (LMICs) should apply principles of
flexibility and adaptability, while maintaining trust through
transparency in exchange, to ensure sustainable and responsive action to
domestic needs within changing contexts. Careful selection and diverse
project team skills, early and continuous engagement with stakeholders,
and robust planning, monitoring and evaluation processes were the
project highlights. Yet, the challenges of political turmoil, changing
Ministry of Health leadership, competing priorities and inactive
coordination mechanisms had to be dealt with. While applying such an
approach of a health system lens to health emergencies in LMICs is
thought to be a success factor in this case, more robust technical
guidance to the nexus implementation is crucial and can be best attained
through encouraging further case reports analysing context-specific
practices.
</td>
<td style="text-align:left;">
Humanitarian-Development Nexus: strengthening health system
preparedness, response and resilience capacities to address COVID-19
in Sudan-case study of repositioning external assistance model
and focus.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Pivoting Continuing Professional Development During the COVID-19
Pandemic: A Narrative Scoping Review of Adaptations and Innovations.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Assessing Primary Care Blood Pressure Documentation for Hypertension
Management During the COVID-19 Pandemic by Patient and Provider Groups.
</td>
</tr>
<tr>
<td style="text-align:left;">
We report a case of fulminant COVID-19-related myocarditis requiring
venoarterial extracorporeal membrane oxygenation where the use of an
ultrasound-enhancing agent demonstrated a previously undescribed
echocardiographic finding, the “lightbulb” sign. This sign potentially
represents a new area for the use of an ultrasound enhancing agent in
the echocardiographic diagnosis of myocarditis.
</td>
<td style="text-align:left;">
The “Lightbulb” Sign: A Novel Echocardiographic Finding Using Ultrasound
Enhancing Agent in Fulminant COVID-19-Related Myocarditis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Outcomes of SARS-CoV-2 infection in early pregnancy-A systematic review
and meta-analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
MVA-BN is an orthopoxvirus vaccine that provides protection against both
smallpox and mpox. In June 2022, Canada launched a publicly-funded
vaccination campaign to offer MVA-BN to at-risk populations including
men who have sex with men (MSM) and sex workers. The safety of MVA-BN
has not been assessed in this context. To address this, the Canadian
National Vaccine Safety Network (CANVAS) conducted prospective safety
surveillance during public health vaccination campaigns in Toronto,
Ontario and in Vancouver, British Columbia. Vaccinated participants
received a survey 7 and 30 days after each MVA-BN dose to elicit adverse
health events. Unvaccinated individuals from a concurrent vaccine safety
project evaluating COVID-19 vaccine safety were used as controls.
Vaccinated and unvaccinated participants that reported a medically
attended visit on their 7-day survey were interviewed. Vaccinated
participants and unvaccinated controls were matched 1:1 based on age
group, gender, sex and provincial study site. Overall, 1,173 vaccinated
participants completed a 7-day survey, of whom 75 % (n = 878) also
completed a 30-day survey. Mild to moderate injection site pain was
reported by 60 % of vaccinated participants. Among vaccinated
participants 8.4 % were HIV positive and when compared to HIV negative
vaccinated individuals, local injection sites were less frequent in
those with HIV (48 % vs 61 %, p = 0.021), but health events preventing
work/school or requiring medical assessment were more frequent (7.1 % vs
3.1 %, p = 0.040). Health events interfering with work/school, or
requiring medical assessment were less common in the vaccinated group
than controls (3.3 % vs. 7.1 %, p &lt; 0.010). No participants were
hospitalized within 7 or 30 days of vaccination. No cases of severe
neurological disease, skin disease, or myocarditis were identified. Our
results demonstrate that the MVA-BN vaccine appears safe when used for
mpox prevention, with a low frequency of severe adverse events and no
hospitalizations observed.
</td>
<td style="text-align:left;">
Prospective monitoring of adverse events following vaccination with
Modified vaccinia Ankara - Bavarian Nordic (MVA-BN) administered to a
Canadian population at risk of Mpox: A Canadian Immunization Research
Network study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of a peer-support programme to improve loneliness and social
isolation due to COVID-19: does adding a secure, user friendly
video-conference solution work better than telephone support alone?
Protocol for a three-arm randomised clinical trial.
</td>
</tr>
<tr>
<td style="text-align:left;">
Pregnant individuals and parents have experienced elevated mental health
problems and stress during COVID-19. Stress during pregnancy can be
harmful to the fetus and detrimental to the parent-child relationship.
However, social support is known to act as a protective factor,
buffering against the adverse effects of stress. The present study
examined whether (1) prenatal stress during COVID-19 was associated with
parent-infant closeness at 6 months postpartum, and (2) social support
moderated the effect of prenatal stress on the parent-infant
relationship. In total, 181 participants completed questionnaires during
pregnancy and at 6 months postpartum. A hierarchical linear regression
analysis was conducted to assess whether social support moderated the
effect of stress during pregnancy on parent-infant closeness at 6 months
postpartum. Results indicated a significant interaction between prenatal
stress and social support on parents’ perceptions of closeness with
their infants at 6 months postpartum (β = .805, p = .029); parents who
experienced high prenatal stress with high social support reported
greater parent-infant closeness, compared to those who reported high
levels of stress and low social support. Findings underscore the
importance of social support in protecting the parent-infant
relationship, particularly in times of high stress, such as during the
COVID-19 pandemic.
</td>
<td style="text-align:left;">
Social support buffers the impact of pregnancy stress on perceptions of
parent-infant closeness during the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Digital Interventions for Stress Among Frontline Health Care Workers:
Results From a Pilot Feasibility Cohort Trial.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
A time-course prediction model of global COVID-19 mortality.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Operational status of mental health, substance use, and problem gambling
services: A system-level snapshot two years into the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
A command centre implementation before and during the COVID-19 pandemic
in a community hospital.
</td>
</tr>
<tr>
<td style="text-align:left;">
In 2020, 149.2 million children worldwide under 5 years suffered from
stunting, and 45.4 million experienced wasting. Many infants are born
already stunted, while others are at high risk for growth faltering
early after birth. Growth faltering is linked to transgenerational
impacts of poverty and marginalization. Few interventions address growth
faltering in infants under 6 months, despite a likely increasing
prevalence due to the negative global economic impacts of the COVID-19
pandemic. Breastfeeding is a critical intervention to alleviate
malnutrition and improve child health outcomes, but rarely receives
adequate attention in growth faltering interventions. A systematic
review and meta-analysis were undertaken to identify and evaluate
interventions addressing growth faltering among infants under 6 months
that employed supplemental milks. The review was carried out following
guidelines from the USA National Academy of Medicine. A total of 10,405
references were identified, and after deduplication 7390 studies were
screened for eligibility. Of these, 227 were assessed for full text
eligibility and relevance. Two randomized controlled trials were
ultimately included, which differed in inclusion criteria and
methodology and had few shared outcomes. Both studies had small sample
sizes, high attrition and high risk of bias. A Bangladeshi study (n =
153) found significantly higher rates of weight gain for F-100 and
diluted F-100 (DF-100) compared with infant formula (IF), while a DRC
trial (n = 146) did not find statistically significant differences in
rate of weight gain for DF-100 compared with IF offered in the context
of broader lactation and relactation support. The meta-analysis of rate
of weight gain showed no statistical difference and some evidence of
moderate heterogeneity. Few interventions address growth faltering among
infants under 6 months. These studies have limited generalizability and
have not comprehensively supported lactation. Greater investment is
necessary to accelerate research that addresses growth faltering
following a new research framework that calls for comprehensive
lactation support.
</td>
<td style="text-align:left;">
Critical research gaps in treating growth faltering in infants under 6
months: A systematic review and meta-analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
RéSUMé: OBJECTIF: Nous avons cherché à mesurer l’influence des
antécédents psychologiques de vaccination sur l’intention de se faire
vacciner contre la COVID-19 chez les citoyennes et citoyens de la Nation
métisse de l’Ontario (NMO). MéTHODE: Un sondage populationnel en ligne a
été mis en œuvre par la NMO quand des vaccins contre la COVID-19 ont été
approuvés au Canada. Les questions posées ont porté sur l’intention de
se faire vacciner, la version abrégée du modèle « 5C » de l’échelle de
vaccination (Confiance, Contraintes, Complaisance, Calcul et
responsabilité Collective) et le profil sociodémographique. Nous avons
utilisé l’échantillonnage fondé sur le recensement via le registre de la
NMO pour obtenir un taux de réponse de 39 %. Des statistiques
descriptives, des analyses bivariées et des modèles de régression
logistique multinomiale (ajustés selon les variables
sociodémographiques) ont servi à analyser les données du sondage.
RéSULTATS: La majorité (70,2 %) des citoyennes et citoyens de la NMO
prévoyaient se faire vacciner. Comparativement aux personnes réticentes
à l’égard de la vaccination, les personnes ayant l’intention de se faire
vacciner avaient plus confiance en l’innocuité des vaccins contre la
COVID-19, considéraient la COVID-19 comme une maladie grave, étaient
disposées à protéger les autres contre la COVID-19 et cherchaient à se
renseigner au sujet des vaccins (Confiance : RC = 19,4, IC95% 15,5–24,2;
Complaisance : RC = 6,21, IC95% 5,38–7,18; responsabilité Collective :
RC = 9,83, IC95% 8,24–11,72; Calcul : RC = 1,43, IC95% 1,28–1,59).
Enfin, les répondantes et les répondants ayant l’intention de se faire
vacciner étaient moins susceptibles de laisser le stress quotidien les
empêcher de se faire vacciner contre la COVID-19 (RC = 0,47, IC95%
0,42–0,53) comparativement aux personnes réticentes à l’égard de la
vaccination. CONCLUSION: Cette étude contribue à la base de
connaissances sur la santé des Métis et a appuyé les activités de
sensibilisation et d’échange d’informations de la NMO pendant le
déploiement des vaccins contre la COVID-19. Une étude future portera sur
la relation entre les « 5C » et le recours réel aux vaccins contre la
COVID-19 chez les citoyennes et citoyens de la NMO.
</td>
<td style="text-align:left;">
COVID-19 vaccination intention and vaccine hesitancy among citizens of
the Métis Nation of Ontario.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Investigating the perceptions and experiences of Canadian dentists on
dental regulatory bodies’ communications and guidelines during the
COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
Leadership is vital to a well-functioning and effective health system.
This importance was underscored during the COVID-19 pandemic. As
disparities in infection and mortality rates became pronounced, greater
calls for equity-informed healthcare emerged. These calls led some
leaders to use the Learning Health System (LHS) approach to quickly
transform research into healthcare practice to mitigate inequities
causing these rates. The LHS is a relatively new framework informed by
many within and outside health systems, supported by decision-makers and
financial arrangements and encouraged by a culture that fosters quick
learning and improvements. Although studies indicate the LHS can enhance
patients’ health outcomes, scarce literature exists on health system
leaders’ use and incorporation of equity into the LHS. This commentary
begins addressing this gap by examining how equity can be incorporated
into LHS activities and discussing ways leaders can ensure equity is
considered and achieved in rapid learning cycles.
</td>
<td style="text-align:left;">
Shifting gears: Creating equity informed leaders for effective learning
health systems.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Appraising the decision-making process concerning COVID-19 policy in
postsecondary education in Canada: A critical scoping review protocol.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Risk factors for COVID-19-associated pulmonary aspergillosis: a
systematic review and meta-analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
The COVID-19 pandemic and lockdowns had significant impacts on sexual
functioning and behavior. Partnered sexual activity decreased overall,
while solo sex activities such as masturbation and pornography
consumption increased exponentially. Given the ongoing debate about the
effects of pornography on sexual function, it was prudent to consider
how the increase in porn consumption during the pandemic might have
impacted sexual function in the post-pandemic period. Results indicated
that despite the increased rates of use during lockdowns, there remains
no evidence supporting the relationship between sexual dysfunction and
porn use during and following the pandemic period. On the contrary,
pornography consumption and solo sex activities offered an alternative
to conventional sexual behavior during a highly stressful period and
were found to have positive effects of relieving psychosocial stress
otherwise induced by the pandemic. Specifically, those who maintained an
active sexual life experienced less anxiety and depression, and greater
relational health than those who were not sexually active. It is
important to consider factors including frequency, context, and type of
consumption when analyzing the impact of pornography on sexual function.
While excessive use can have negative effects, moderate use can be a
natural and healthy part of life.
</td>
<td style="text-align:left;">
Pornography and sexual function in the post-pandemic period: a narrative
review from psychological, psychiatric, and sexological perspectives.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Healthcare providers’ perspectives on implementing a brief physical
activity and diet intervention within a primary care smoking cessation
program: a qualitative study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Novel drop-in web-based mindfulness sessions (Pause-4-Providers) to
enhance well-being amongst healthcare workers during the COVID-19
Pandemic: a descriptive and qualitative study.
</td>
</tr>
<tr>
<td style="text-align:left;">
COVID-19 associated public health measures and school closures
exacerbated symptoms in some children and youth with attention-deficit
hyperactivity disorder (ADHD). Less well understood is how the pandemic
influenced patterns of prescription stimulant use. We conducted a
population-based study of stimulant dispensing to children and
youth ≤ 24 years old between January 1, 2013, and June 30, 2022. We used
structural break analyses to identify the pandemic month(s) when changes
in the dispensing of stimulants occurred. We used interrupted time
series models to quantify changes in dispensing following the structural
break and compare observed and expected stimulant use. Our main outcome
was the change in the monthly rate of stimulant use per 100,000 children
and youth. Following an initial immediate decline of 60.1 individuals
per 100,000 (95% confidence interval \[CI\] - 99.0 to - 21.2), the
monthly rate of stimulant dispensing increased by 11.8 individuals per
100,000 (95% CI 10.0-13.6), with the greatest increases in trend
observed among females, individuals in the highest income
neighbourhoods, and those aged 20 to 24. Observed rates were between
3.9% (95% CI 1.7-6.2%) and 36.9% (95% CI 34.3-39.5%) higher than
predicted among females from June 2020 onward and between 7.1% (95% CI
4.2-10.0%) and 50.7% (95% CI 47.0-54.4%) higher than expected among
individuals aged 20-24 from May 2020 onward. Additional research is
needed to ascertain the appropriateness of stimulant use and to develop
strategies supporting children and youth with ADHD during future periods
of long-term stressors.
</td>
<td style="text-align:left;">
Impact of COVID-19 pandemic on prescription stimulant use among children
and youth: a population-based study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Co-designing solutions to enhance access and engagement in pediatric
telerehabilitation.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Dental service utilization and the COVID-19 pandemic, a micro-data
analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of the COVID-19 pandemic on prescription drug use and costs in
British Columbia: a retrospective interrupted time series study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
We have reached single-visit testing, diagnosis, and treatment for
hepatitis C infection, now what?
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Clinical decision support to enhance venous thromboembolism
pharmacoprophylaxis prescribing for pediatric inpatients with COVID-19.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Transitions of care for older adults discharged home from the emergency
department: an inductive thematic content analysis of patient comments.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
“I shall not poison my child with your human experiment”: Investigating
predictors of parents’ hesitancy about vaccinating younger children
(&lt;12) in Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Trends in outpatient and inpatient visits for separate
ambulatory-care-sensitive conditions during the first year of the
COVID-19 pandemic: a province-based study.
</td>
</tr>
<tr>
<td style="text-align:left;">
Eating disorders disproportionally affect children and youth, however,
literature investigating long-term trends of eating disorder
hospitalizations among children and youth in Canada is limited. We
conducted a retrospective surveillance study, examining eating disorder
hospitalizations among children and youth in Canada, from 2010 to 2022,
by sex, age group, geography and eating disorder diagnosis. More than
half of eating disorder hospitalizations examined during our study
period were first-time hospitalizations. The most common eating disorder
diagnoses were anorexia nervosa, followed by unspecified eating
disorders. Youth had higher rates of eating disorders compared to
younger children and females had higher rates compared to males. In
Canada, rates of pediatric eating disorder hospitalizations increased
during the pandemic. These results emphasize the need for continued
surveillance to monitor how ED hospitalizations evolve post-pandemic, as
well as prioritizing early intervention and treatment to help reduce the
number of children and youth requiring hospitalization.
</td>
<td style="text-align:left;">
Eating disorder hospitalizations among children and youth in Canada from
2010 to 2022: a population-based surveillance study using administrative
data.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Burnout among public health workers in Canada: a cross-sectional study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Long-Term Safety of Dupilumab in Patients With Moderate-to-Severe
Asthma: TRAVERSE Continuation Study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The association between frailty, long-term care home characteristics and
COVID-19 mortality before and after SARS-CoV-2 vaccination: a
retrospective cohort study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
COVID-19 outcomes in patients with sickle cell disease and sickle cell
trait compared with individuals without sickle cell disease or trait: a
systematic review and meta-analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
Individuals living with tetralogy of Fallot require lifelong specialized
congenital heart disease care to monitor for and manage potential late
complications. However, access to cardiology care remains a challenge
for many patients, as does access to mental health services, dental
care, obstetrical care, and other specialties required by this
population. Inequities in health care access were highlighted by the
COVID-19 pandemic and continue to exist. Paradoxically, many social
factors influence an individual’s need for care, yet inadvertently
restrict access to it. These include sex and gender, being a member of a
racial or ethnic historically excluded group, lower educational
attainment, lower socioeconomic status, living remotely from tertiary
care centres, transportation difficulties, inadequate health insurance,
occupational instability, and prior experiences with discrimination in
the health care setting. These factors may coexist and have compounding
effects. In addition, many patients believe that they are cured and
unaware of the need for specialized follow-up. For these reasons, lapses
in care are common, particularly around the time of transfer from
paediatric to adult care. The lack of trained health care professionals
for adults with congenital heart disease presents an additional barrier,
even in higher income countries. This review summarizes challenges
regarding access to multiple domains of specialized care for individuals
with tetralogy of Fallot, with a focus on the impact of social
determinants of health. Specific recommendations to improve access to
care within Canadian and American systems are offered.
</td>
<td style="text-align:left;">
Access to Specialized Care Across the Lifespan in Tetralogy of Fallot.
</td>
</tr>
<tr>
<td style="text-align:left;">
Kawasaki disease (KD) and Multisystem Inflammatory Syndrome in Children
(MIS-C) associated with COVID-19 show clinical overlap and both lack
definitive diagnostic testing, making differentiation challenging. We
sought to determine how cardiac biomarkers might differentiate KD from
MIS-C. The International Kawasaki Disease Registry enrolled
contemporaneous KD and MIS-C pediatric patients from 42 sites from
January 2020 through June 2022. The study population included 118 KD
patients who met American Heart Association KD criteria and compared
them to 946 MIS-C patients who met 2020 Centers for Disease Control and
Prevention case definition. All included patients had at least one
measurement of amino-terminal prohormone brain natriuretic peptide
(NTproBNP) or cardiac troponin I (TnI), and echocardiography. Regression
analyses were used to determine associations between cardiac biomarker
levels, diagnosis, and cardiac involvement. Higher NTproBNP
(≥ 1500 ng/L) and TnI (≥ 20 ng/L) at presentation were associated with
MIS-C versus KD with specificity of 77 and 89%, respectively. Higher
biomarker levels were associated with shock and intensive care unit
admission; higher NTproBNP was associated with longer hospital length of
stay. Lower left ventricular ejection fraction, more pronounced for
MIS-C, was also associated with higher biomarker levels. Coronary artery
involvement was not associated with either biomarker. Higher NTproBNP
and TnI levels are suggestive of MIS-C versus KD and may be clinically
useful in their differentiation. Consideration might be given to their
inclusion in the routine evaluation of both conditions.
</td>
<td style="text-align:left;">
Cardiac Biomarkers Aid in Differentiation of Kawasaki Disease from
Multisystem Inflammatory Syndrome in Children Associated with COVID-19.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Workforce resilience supporting staff in managing stress: A coherent
breathing intervention for the long-term care workforce.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of pandemic on use of mechanical chest compression systems.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Post-COVID-19 Condition in Children 6 and 12 Months After Infection.
</td>
</tr>
<tr>
<td style="text-align:left;">
On 24 February 2022, Russia attacked Ukraine. Millions of people tuned
into social media to watch the war. Media exposure to disasters and
large-scale violence can precipitate anxiety resulting in intrusive
thoughts. This research investigates factors related to anxiety while
watching the war. Since the war began during the ongoing coronavirus
pandemic, threat from COVID-19 is seen as a predictor of anxiety when
watching the war. A theoretical model is put forward where the outcome
was anxiety when watching the war, and predictors were self-reported
interference of watching the war with one’s studies or work, gender,
worry about the war, self-efficacy and coronavirus threat. Data were
collected online with independent samples of university students from
two European countries close to Ukraine, Germany (n = 348) and Finland
(n = 228), who filled out an anonymous questionnaire. Path analysis was
used to analyse the data. Findings showed that the model was an
acceptable fit to the data in each sample, and standardised regression
coefficients indicated that anxiety, when watching the war, increased
with interference, war worry and coronavirus threat, and decreased with
self-efficacy. Women reported more anxiety when watching the war than
men. Implications of the results are discussed.
</td>
<td style="text-align:left;">
Anxiety and watching the war in Ukraine.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Examining the Relationship Between Workplace Industry and COVID-19
Infection: A Cross-sectional Study of Canada’s Largest Rapid Antigen
Screening Program.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Virtual Visits With Own Family Physician vs Outside Family Physician and
Emergency Department Use.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The role of the neutrophil-lymphocyte ratio in predicting poor outcomes
in COVID-19 patients.
</td>
</tr>
<tr>
<td style="text-align:left;">
The coronavirus pandemic (COVID-19) has placed incredible demands on
healthcare workers (HCWs) and adversely impacted their well-being.
Throughout the pandemic, organizations have sought to implement brief
and flexible mental health interventions to better support employees.
Few studies have explored HCWs’ lived experiences of participating in
brief, online mindfulness programming during the pandemic using
qualitative methodologies. To address this gap, we conducted
semi-structured interviews with HCWs and program facilitators
(\<i\>n\</i\> = 13) who participated in an online, four-week,
mindfulness-based intervention program. The goals of this study were to:
(1) understand how participants experienced work during the pandemic;
(2) understand how the rapid switch to online life impacted program
delivery and how participants experienced the mindfulness program; and
(3) describe the role of the mindfulness program in supporting
participants’ mental health and well-being. We utilized interpretive
phenomenological analysis (IPA) to elucidate participants’ and
facilitators’ rich and meaningful lived experiences and identified
patterns of experiences through a cross-case analysis. This resulted in
four main themes: (1) changing environments; (2) snowball of emotions;
(3) connection and disconnection; and (4) striving for resilience.
Findings from this study highlight strategies for organizations to
create and support wellness programs for HCWs in times of public health
crises. These include improving social connection in virtual care
settings, providing professional development and technology training for
HCWs to adapt to rapid environmental changes, and recognizing the
difference between emotions and emotional states in HCWs involved in
mindfulness-based programs.
</td>
<td style="text-align:left;">
Exploring the lived experiences of participants and facilitators of an
online mindfulness program during COVID-19: a phenomenological study.
</td>
</tr>
<tr>
<td style="text-align:left;">
This systematic review and meta-analysis of randomised controlled trials
(RCTs) aimed to evaluate the efficacy, safety, and tolerability of
fluvoxamine for the outpatient management of COVID-19. We conducted this
review in accordance with the PRISMA 2020 guidelines. Literature
searches were conducted in MEDLINE, EMBASE, International Pharmaceutical
Abstracts, CINAHL, Web of Science, and CENTRAL up to 14 September 2023.
Outcomes included incidence of hospitalisation, healthcare utilization
(emergency room visits and/or hospitalisation), mortality, supplemental
oxygen and mechanical ventilation requirements, serious adverse events
(SAEs) and non-adherence. Fluvoxamine 100 mg twice a day was associated
with reductions in the risk of hospitalisation (risk ratio \[RR\] 0.75,
95% confidence interval \[CI\] 0.58-0.97; I \<sup\>2\</sup\>  = 0%) and
reductions in the risk of healthcare utilization (RR 0.68, 95% CI
0.53-0.86; I \<sup\>2\</sup\>  = 0%). While no increased SAEs were
observed, fluvoxamine 100 mg twice a day was associated with higher
treatment non-adherence compared to placebo (RR 1.61, 95% CI 1.22-2.14;
I \<sup\>2\</sup\>  = 53%). In subgroup analyses, fluvoxamine reduced
healthcare utilization in outpatients with BMI ≥30 kg/m\<sup\>2\</sup\>
, but not in those with lower BMIs. While fluvoxamine offers potential
benefits in reducing healthcare utilization, its efficacy may be most
pronounced in high-risk patient populations. The observed non-adherence
rates highlight the need for better patient education and counselling.
Future investigations should reassess trial endpoints to include
outcomes relating to post-COVID sequelaes. Registration: This review was
prospectively registered on PROSPERO (CRD42023463829).
</td>
<td style="text-align:left;">
Evaluating fluvoxamine for the outpatient treatment of COVID-19: A
systematic review and meta-analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of the COVID-19 pandemic on antidepressant and antipsychotic use
among children and adolescents: a population-based study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Design of a Dyadic Digital Health Module for Chronic Disease Shared
Care: Development Study.
</td>
</tr>
<tr>
<td style="text-align:left;">
As the COVID-19 pandemic unfolded, community pharmacies adapted rapidly
to broaden and adjust the services they were providing to patients,
while coping with severe pressure on supply chains and constrained
social interactions. This study investigates whether these events had an
impact on the medication incidents reported by pharmacists. Results
indicate that Canadian pharmacies were able to sustain such stress while
maintaining comparable safety levels. At the same time, it appears that
some risk factors that were either ignored or not meaningful in the past
started to be reported, suggesting that community pharmacists are now
aware of a larger set of contributing factors that can lead to
medication incidents, notably for medication incidents that can lead to
harm.
</td>
<td style="text-align:left;">
Investigating the impact of the COVID-19 pandemic on the occurrence of
medication incidents in Canadian community pharmacies.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Adaptive servo-ventilation for sleep-disordered breathing in patients
with heart failure with reduced ejection fraction (ADVENT-HF): a
multicentre, multinational, parallel-group, open-label, phase 3
randomised controlled trial.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Environment-based approaches to improve participation of young people
with physical disabilities during COVID-19.
</td>
</tr>
<tr>
<td style="text-align:left;">
Severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2), infamously
known as Coronavirus Disease 2019 (COVID-19), is responsible for the
current pandemic and, to date, has greatly impacted public health and
economy globally \[…\].
</td>
<td style="text-align:left;">
Global Analysis of Tracking the Evolution of SARS-CoV-2 Variants.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Faith-based organisations and their role in supporting vaccine
confidence and uptake: a scoping review protocol.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Examining adaptive models of care implemented in hospital ICUs during
the COVID-19 pandemic: a qualitative study.
</td>
</tr>
<tr>
<td style="text-align:left;">
Neurological manifestations have been widely reported in adults with
COVID-19, yet the extent of involvement among the pediatric population
is currently poorly characterized. The objective of our systematic
review is to evaluate the association of SARS-CoV-2 infection with
neurological symptoms and neuroimaging manifestations in the pediatric
population. A literature search of Cochrane Library; EBSCO CINAHL;
Global Index Medicus; OVID AMED, Embase, Medline, PsychINFO; and Scopus
was conducted in accordance with the Peer Review of Electronic Search
Strategies form (October 1, 2019 to March 15, 2022). Studies were
included if they reported (1) COVID-19-associated neurological symptoms
and neuroimaging manifestations in individuals aged &lt;18 years with a
confirmed, first SARS-CoV-2 infection and were (2) peer-reviewed.
Full-text reviews of 222 retrieved articles were performed, along with
subsequent reference searches. A total of 843 no-duplicate records were
retrieved. Of the 19 identified studies, there were ten retrospective
observational studies, seven case series, one case report, and one
prospective cohort study. A total of 6985 individuals were included,
where 12.8% (n = 892) of hospitalized patients experienced
neurocognitive impairments which includes: 1) neurological symptoms
(n = 294 of 892, 33.0%), 2) neurological syndromes and neuroimaging
abnormalities (n = 223 of 892, 25.0%), and 3) other phenomena (n = 233
of 892, 26.1%). Based on pediatric-specific cohorts, children
experienced more drowsiness (7.3% vs. 1.3%) and muscle weakness (7.3%
vs. 6.3%) as opposed to adolescents. Agitation or irritability was
observed more in children (7.3%) than infants (1.3%). Our findings
revealed a high prevalence of immune-mediated patterns of disease among
COVID-19 positive pediatric patients with neurocognitive abnormalities.
</td>
<td style="text-align:left;">
Association of SARS-CoV-2 infection with neurological impairments in
pediatric population: A systematic review.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
COVID-19 Infection, Symptoms, and Stroke Revascularization Outcomes:
Intriguing Connections.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Scheduled and urgent inguinal hernia repair in Ontario, Canada between
2010 and 2022: Population-based cross sectional analysis of trends and
outcomes.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Yoga Pose Estimation Using Angle-Based Feature Extraction.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Patient Presentations in a Community Pain Clinic after COVID-19
Infection or Vaccination: A Case-Series Approach.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
What are effective strategies to respond to the psychological impacts of
working on the frontlines of a public health emergency?
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Association between biochemical and hematologic factors with COVID-19
using data mining methods.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Long COVID in long-term care: a rapid realist review.
</td>
</tr>
<tr>
<td style="text-align:left;">
The COVID-19 pandemic has led to an increase in the prevalence of
substance use presentations. This study aims to assess the impact of the
COVID-19 pandemic on the rate of primary care visits for substance use
including tobacco, alcohol, and other drug use among primary care
patients in Ontario, Canada. Diagnostic and service fee code data were
collected from a longitudinal cohort of family medicine patients during
pre-pandemic (March 14, 2019-March 13, 2020) and pandemic periods (March
14, 2020-March 13, 2021). Generalized linear models were used to compare
the rate of substance-use related visits pre-pandemic and during the
pandemic. The effects of demographic characteristics including age, sex,
and income quintile were also assessed. Relative to the pre-pandemic
period, patients were less likely to have a primary care visit during
the pandemic for tobacco-use related reasons (OR = 0.288, 95% CI
\[0.270-0.308\]), and for alcohol-use related reasons (OR = 0.851, 95%
CI \[0.780-0.929\]). In contrast, patients were more likely to have a
primary care visit for other drug-use related reasons (OR = 1.150, 95%
CI \[1.080-1.225\]). In the face of a known increase in substance use
during the COVID-19 pandemic, a decrease in substance use-related
primary care visits likely represents an unmet need for this patient
population. This study highlights the importance of continued research
in the field of substance use, especially in periods of heightened
vulnerability such as during the COVID-19 pandemic.
</td>
<td style="text-align:left;">
The impact of the COVID-19 pandemic on the rate of primary care visits
for substance use among patients in Ontario, Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Metabolic disturbances potentially attributable to clogging during
continuous renal replacement therapy.
</td>
</tr>
<tr>
<td style="text-align:left;">
Disease spread can be affected by pharmaceutical interventions (such as
vaccination) and non-pharmaceutical interventions (such as physical
distancing, mask-wearing and contact tracing). Understanding the
relationship between disease dynamics and human behaviour is a
significant factor to controlling infections. In this work, we propose a
compartmental epidemiological model for studying how the infection
dynamics of COVID-19 evolves for people with different levels of social
distancing, natural immunity and vaccine-induced immunity. Our model
recreates the transmission dynamics of COVID-19 in Ontario up to
December 2021. Our results indicate that people change their behaviour
based on the disease dynamics and mitigation measures. Specifically,
they adopt more protective behaviour when mandated social distancing
measures are in effect, typically concurrent with a high number of
infections. They reduce protective behaviour when vaccination coverage
is high or when mandated contact reduction measures are relaxed,
typically concurrent with a reduction of infections. We demonstrate that
waning of infection and vaccine-induced immunity are important for
reproducing disease transmission in autumn 2021.
</td>
<td style="text-align:left;">
Pharmaceutical and non-pharmaceutical interventions for controlling the
COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
Stress is associated with numerous chronic health conditions, both
mental and physical. However, the heterogeneity of these associations at
the individual level is poorly understood. While data generated from
individuals in their day-to-day lives “in the wild” may best represent
the heterogeneity of stress, gathering these data and separating signals
from noise is challenging. In this work, we report findings from a major
data collection effort using Digital Health Technologies (DHTs) and
frontline healthcare workers. We provide insights into stress “in the
wild”, by using robust methods for its identification from multimodal
data and quantifying its heterogeneity. Here we analyze data from the
Stress and Recovery in Frontline COVID-19 Workers study following 365
frontline healthcare workers for 4-6 months using wearable devices and
smartphone app-based measures. Causal discovery is used to learn how the
causal structure governing an individual’s self-reported symptoms and
physiological features from DHTs differs between non-stress and
potential stress states. Our methods uncover robust representations of
potential stress states across a population of frontline healthcare
workers. These representations reveal high levels of inter- and
intra-individual heterogeneity in stress. We leverage multiple stress
definitions that span different modalities (from subjective to
physiological) to obtain a comprehensive view of stress, as these
differing definitions rarely align in time. We show that these different
stress definitions can be robustly represented as changes in the
underlying causal structure on and off stress for individuals. This
study is an important step toward better understanding potential
underlying processes generating stress in individuals.
</td>
<td style="text-align:left;">
Dissecting the heterogeneity of “in the wild” stress from multimodal
sensor data.
</td>
</tr>
<tr>
<td style="text-align:left;">
At the beginning of the COVID-19 pandemic, the Kenya Ministry of Health
instituted movement cessation measures and limits on face-to-face
meetings. We explore the ways in which female sex workers (FSWs) in
Nairobi were affected by the COVID-19 control measures and the ways they
coped with the hardships. Forty-seven women were randomly sampled from
the Maisha Fiti study, a longitudinal study of 1003 FSWs accessing
sexual reproductive health services in Nairobi for an in-depth
qualitative interview 4-5 months into the pandemic. We sought to
understand the effects of COVID-19 on their lives. Data were
transcribed, translated, and coded inductively. The COVID-19 measures
disenfranchised FSWs reducing access to healthcare, decreasing income
and increasing sexual, physical, and financial abuse by clients and law
enforcement. Due to the customer-facing nature of their work, sex
workers were hit hard by the COVID-19 restrictions. FSWs experienced
poor mental health and strained interpersonal relationships. To cope
they skipped meals, reduced alcohol use and smoking, started small
businesses to supplement sex work or relocated to their rural homes.
Interventions that ensure continuity of access to health services,
prevent exploitation, and ensure the social and economic protection of
FSWs during times of economic strain are required.
</td>
<td style="text-align:left;">
Surviving pandemic control measures: The experiences of female sex
workers during COVID-19 in Nairobi, Kenya.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Covid-19 Vaccine Hesitancy and Under-Vaccination among Marginalized
Populations in the United States and Canada: A Scoping Review.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The Effects of Cognitive Ability, Mental Health, and Self-Quarantining
on Functional Ability of Older Adults During the COVID-19 Pandemic:
Results From the Canadian Longitudinal Study on Aging.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of Elevated Body Mass Index (BMI) on Hedonic Tone in Persons with
Post-COVID-19 Condition: A Secondary Analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Initiations of safer supply hydromorphone increased during the COVID-19
pandemic in Ontario: An interrupted time series analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Patient and families’ perspectives on telepalliative care: A systematic
integrative review.
</td>
</tr>
<tr>
<td style="text-align:left;">
Telehealth and telemedicine have encountered explosive growth since the
beginning of the COVID-19 pandemic, resulting in increased access to
care for patients located far from medical centers and clinics.
Subspecialty clinicians in behavioral neurology &amp; neuropsychiatry
(BNNP) have implemented the use of telemedicine platforms to perform
cognitive examinations that were previously office based. In this
perspective article, BNNP clinicians at Massachusetts General Hospital
(MGH) describe their experience performing cognitive examinations via
telemedicine. The article reviews the goals, prerequisites, advantages,
and potential limitations of performing a video- or telephone-based
telemedicine cognitive examination. The article shares the approaches
used by MGH BNNP clinicians to examine cognitive and behavioral areas,
such as orientation, attention and executive functions, language, verbal
learning and memory, visual learning and memory, visuospatial function,
praxis, and abstract abilities, as well as to survey for
neuropsychiatric symptoms and assess activities of daily living.
Limitations of telemedicine-based cognitive examinations include limited
access to and familiarity with telecommunication technologies on the
patient side, limitations of the technology itself on the clinician
side, and the limited psychometric validation of virtual assessments.
Therefore, an in-person examination with a BNNP clinician or a formal
in-person neuropsychological examination with a neuropsychologist may be
recommended. Overall, this article emphasizes the use of standardized
cognitive and behavioral assessment instruments that are either in the
public domain or, if copyrighted, are nonproprietary and do not require
a fee to be used by the practicing BNNP clinician.
</td>
<td style="text-align:left;">
Telemedicine-Based Cognitive Examinations During COVID-19 and Beyond:
Perspective of the Massachusetts General Hospital Behavioral Neurology
&amp; Neuropsychiatry Group.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
What motivates individuals to share information with governments when
adopting health technologies during the COVID-19 pandemic?
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Disruption to Pattern but No Overall Increase in the Expected Incidence
of Pediatric Diabetes During the First Three Years of the COVID-19
Pandemic in Ontario, Canada (March 2020-March 2023).
</td>
</tr>
<tr>
<td style="text-align:left;">
The COVID-19 pandemic has mandated a re-imagination of how healthcare is
administered and delivered, with a view towards focusing on
person-centred care and advancing population health while increasing
capacity, access and equity in the healthcare system. These goals can be
achieved through healthcare integration. In 2019, the University Health
Network (UHN), a consortium of four quaternary care hospitals in
Ontario, Canada, established the first stage of a pilot program to
increase healthcare integration at the institutional level and
vertically with other primary, secondary and tertiary institutions in
the Ontario healthcare system. Implementation of the program was
accelerated during the COVID-19 pandemic and demonstrated how healthcare
integration improves person-centred care and population health;
therefore serving as the foundation for a health system response for the
COVID-19 pandemic recovery and beyond.
</td>
<td style="text-align:left;">
Integrated Care: A Person-Centered and Population Health Strategy for
the COVID-19 Pandemic Recovery and Beyond.
</td>
</tr>
<tr>
<td style="text-align:left;">
Using qualitative interviews with 68 family physicians (FPs) in Canada,
we describe practice- and system-based approaches that were used to
mitigate COVID-19 exposure in primary care settings across Canada to
ensure the continuation of primary care delivery. Participants described
how they applied infection prevention and control procedures (risk
assessment, hand hygiene, control of environment, administrative
control, personal protective equipment) and relied on centralized
services that directed patients with COVID-19 to settings outside of
primary care, such as testing centres. The multi-layered approach
mitigated the risk of COVID-19 exposure while also conserving resources,
preserving capacity and supporting supply chains.
</td>
<td style="text-align:left;">
Practice- and System-Based Interventions to Reduce COVID-19 Transmission
in Primary Care Settings: A Qualitative Study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Decision making in continuing professional development organisations
during a crisis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The impact of COVID-19 three years on: Introduction to the 2023 special
issue.
</td>
</tr>
<tr>
<td style="text-align:left;">
The COVID-19 pandemic appears to have shifted the care trajectories of
many residents and care partners in Ontario who considered leaving LTC
to live in the community for a portion or the duration of the pandemic.
This type of care transition-from LTC to home care-was highly uncommon
prior to the pandemic, therefore we know relatively little about the
planning and decision-making involved. The aim of this study was to
describe who was involved in LTC to home care transitions in Ontario
during the COVID-19 pandemic, to what extent, and the factors that
guided their decision-making. A qualitative description study involving
semi-structured interviews with 32 residents, care partners and health
professionals was conducted. Transition decisions were largely made by
care partners, with varied input from residents or health professionals.
Stakeholders considered seven factors, previously identified in a
scoping review, when making their transition decisions: (a)
institutional priorities and requirements; (b) resources; (c) knowledge;
(d) risk; (e) group structure and dynamic; (f) health and support needs;
and (g) personality preferences and beliefs. Participants’ emotional
responses to the pandemic also influenced the perceived need to pursue a
care transition. The findings of this research provide insights towards
the planning required to support LTC to home care transitions, and the
many challenges that arise during decision-making.
</td>
<td style="text-align:left;">
Long-term care transitions during a global pandemic: Planning and
decision-making of residents, care partners, and health professionals in
Ontario, Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The Evolving Roles and Expectations of Inpatient Palliative Care Through
COVID-19: a Systematic Review and Meta-synthesis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Effectiveness of a Fourth COVID-19 mRNA Vaccine Dose Against the Omicron
Variant in Solid Organ Transplant Recipients.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The mental health impacts of the COVID-19 pandemic among individuals
with depressive, anxiety, and stressor-related disorders: A scoping
review.
</td>
</tr>
<tr>
<td style="text-align:left;">
Scientific evidence regularly guides policy decisions\<sup\>1\</sup\>,
with behavioural science increasingly part of this
process\<sup\>2\</sup\>. In April 2020, an influential
paper\<sup\>3\</sup\> proposed 19 policy recommendations (‘claims’)
detailing how evidence from behavioural science could contribute to
efforts to reduce impacts and end the COVID-19 pandemic. Here we assess
747 pandemic-related research articles that empirically investigated
those claims. We report the scale of evidence and whether evidence
supports them to indicate applicability for policymaking. Two
independent teams, involving 72 reviewers, found evidence for 18 of 19
claims, with both teams finding evidence supporting 16 (89%) of those 18
claims. The strongest evidence supported claims that anticipated
culture, polarization and misinformation would be associated with policy
effectiveness. Claims suggesting trusted leaders and positive social
norms increased adherence to behavioural interventions also had strong
empirical support, as did appealing to social consensus or bipartisan
agreement. Targeted language in messaging yielded mixed effects and
there were no effects for highlighting individual benefits or protecting
others. No available evidence existed to assess any distinct differences
in effects between using the terms ‘physical distancing’ and ‘social
distancing’. Analysis of 463 papers containing data showed generally
large samples; 418 involved human participants with a mean of 16,848
(median of 1,699). That statistical power underscored improved
suitability of behavioural science research for informing policy
decisions. Furthermore, by implementing a standardized approach to
evidence selection and synthesis, we amplify broader implications for
advancing scientific evidence in policy formulation and prioritization.
</td>
<td style="text-align:left;">
A synthesis of evidence for policy from behavioural science during
COVID-19.
</td>
</tr>
<tr>
<td style="text-align:left;">
School-based health promotion is drastically disrupted by school
closures during public health emergencies or natural disasters. Climate
change will likely accelerate the frequency of these events and hence
school closures. We identified innovative health promotion practices
delivered during COVID-19 school closures and sought consensus among
education experts on their future utility. Fifteen health promotion
practices delivered in 87 schools across Alberta, Canada during COVID-19
school closures in Spring 2020, were grouped into: ‘awareness of healthy
lifestyle behaviours and mental wellness’, ‘virtual events’, ‘tangible
supports’ and ‘school-student-family connectedness’. Two expert panels
(23 school-level practitioners and 20 decision-makers at the school
board and provincial levels) rated practices on feasibility,
acceptability, reach, effectiveness, cost-effectiveness and other
criteria in three rounds of online Delphi surveys. Consensus was reached
if 70% or more participants (strongly) agreed with a statement,
(strongly) disagreed or neither. Participants agreed all practices
require planning, preparation and training before implementation and
additional staff time and most require external support or partnerships.
Participants rated ‘awareness of healthy lifestyle behaviours and mental
wellness’ and ‘virtual events’ as easy and quick to implement, effective
and cost-effective, sustainable, easy to integrate into curriculum, well
received by students and teachers, benefit school culture and require no
additional funding/resources. ‘Tangible supports’ (equipment, food) and
‘school-student-family connectedness’ were rated as most likely to reach
vulnerable students and families. Health promotion practices presented
herein can inform emergency preparedness plans and are critical to
ensuring health remains a priority during public health emergencies and
natural disasters.
</td>
<td style="text-align:left;">
Delivering health promotion during school closures in public health
emergencies: building consensus among Canadian experts.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Integration of hospital with congregate care homes in response to the
COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
“None of us are lying”: an interpretive description of the search for
legitimacy and the journey to access quality health services by
individuals living with Long COVID.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of Antenatal Care Modifications on Gestational Diabetes Outcomes
During the Corona Virus 19 Pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Unintentional pediatric poisonings before and during the COVID-19
pandemic: A population-based study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Defining the Clinicoradiologic Syndrome of SARS-CoV-2 Acute Necrotizing
Encephalopathy: A Systematic Review and 3 New Pediatric Cases.
</td>
</tr>
<tr>
<td style="text-align:left;">
While previous reviews found a positive association between pre-existing
cancer diagnosis and COVID-19-related death, most early studies did not
distinguish long-term cancer survivors from those recently
diagnosed/treated, nor adjust for important confounders including age.
We aimed to consolidate higher-quality evidence on risk of
COVID-19-related death for people with recent/active cancer (compared to
people without) in the pre-COVID-19-vaccination period. We searched the
WHO COVID-19 Global Research Database (20 December 2021), and Medline
and Embase (10 May 2023). We included studies adjusting for age and sex,
and providing details of cancer status. Risk-of-bias assessment was
based on the Newcastle-Ottawa Scale. Pooled adjusted odds or risk ratios
(aORs, aRRs) or hazard ratios (aHRs) and 95% confidence intervals (95%
CIs) were calculated using generic inverse-variance random-effects
models. Random-effects meta-regressions were used to assess associations
between effect estimates and time since cancer diagnosis/treatment. Of
23 773 unique title/abstract records, 39 studies were eligible for
inclusion (2 low, 17 moderate, 20 high risk of bias). Risk of
COVID-19-related death was higher for people with active or recently
diagnosed/treated cancer (general population: aOR = 1.48, 95% CI:
1.36-1.61, I\<sup\>2\</sup\>  = 0; people with COVID-19: aOR = 1.58, 95%
CI: 1.41-1.77, I\<sup\>2\</sup\>  = 0.58; inpatients with COVID-19:
aOR = 1.66, 95% CI: 1.34-2.06, I\<sup\>2\</sup\>  = 0.98). Risks were
more elevated for lung (general population: aOR = 3.4, 95% CI: 2.4-4.7)
and hematological cancers (general population: aOR = 2.13, 95% CI:
1.68-2.68, I\<sup\>2\</sup\>  = 0.43), and for metastatic cancers.
Meta-regression suggested risk of COVID-19-related death decreased with
time since diagnosis/treatment, for example, for any/solid cancers,
fitted aOR = 1.55 (95% CI: 1.37-1.75) at 1 year and aOR = 0.98 (95% CI:
0.80-1.20) at 5 years post-cancer diagnosis/treatment. In conclusion,
before COVID-19-vaccination, risk of COVID-19-related death was higher
for people with recent cancer, with risk depending on cancer type and
time since diagnosis/treatment.
</td>
<td style="text-align:left;">
Risk of COVID-19 death for people with a pre-existing cancer diagnosis
prior to COVID-19-vaccination: A systematic review and meta-analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Publisher Correction: Early corticosteroids are associated with lower
mortality in critically ill patients with COVID‑19: a cohort study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Forensic investigations of disasters: Past achievements and new
directions.
</td>
</tr>
<tr>
<td style="text-align:left;">
Awareness of RNA-based therapies has increased after the widespread
adoption of mRNA vaccines against SARS-CoV-2 during the COVID-19
pandemic. These mRNA vaccines had a significant impact on reducing lung
disease and mortality. They highlighted the potential for rapid
development of RNA-based therapies and advances in nanoparticle delivery
systems. Along with the rapid advancement in RNA biology, including the
description of noncoding RNAs as major products of the genome, this
success presents an opportunity to highlight the potential of RNA as a
therapeutic modality. Here, we review the expanding compendium of
RNA-based therapies, their mechanisms of action and examples of
application in the lung. The airways provide a convenient conduit for
drug delivery to the lungs with decreased systemic exposure. This review
will also describe other delivery methods, including local delivery to
the pleura and delivery vehicles that can target the lung after systemic
administration, each providing access options that are advantageous for
a specific application. We present clinical trials of RNA-based therapy
in lung disease and potential areas for future directions. This review
aims to provide an overview that will bring together researchers and
clinicians to advance this burgeoning field.
</td>
<td style="text-align:left;">
Unlocking the potential of RNA-based therapeutics in the lung: current
status and future directions.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Eleven-month SARS-CoV-2 binding antibody decay, and associated factors,
among mRNA vaccinees: implications for booster vaccination.
</td>
</tr>
<tr>
<td style="text-align:left;">
The coronavirus disease 2019 (COVID-19) pandemic, caused by the
SARS-CoV-2 virus, has rapidly evolved since late 2019, due to highly
transmissible Omicron variants. While most Canadian paramedics have
received COVID-19 vaccination, the optimal ongoing vaccination strategy
is unclear. We investigated neutralizing antibody (NtAb) response
against wild-type (WT) Wuhan Hu-1 and Omicron BA.4/5 lineages based on
the number of doses and past SARS-CoV-2 infection, at 18 months
post-initial vaccination (with a Wuhan Hu-1 platform mRNA vaccine
\[BNT162b2 or mRNA-1273\]). Demographic information, previous COVID-19
vaccination, infection history, and blood samples were collected from
paramedics 18 months post-initial mRNA COVID-19 vaccine dose. Outcome
measures were ACE2 percent inhibition against Omicron BA.4/5 and WT
antigens. We compared outcomes based on number of vaccine doses (two
vs. three) and previous SARS-CoV-2 infection status, using the
Mann-Whitney U test. Of 657 participants, the median age was 40 years
(IQR 33-50) and 251 (42 %) were females. Overall, median percent
inhibition to BA.4/5 and WT was 71.61 % (IQR 39.44-92.82) and 98.60 %
(IQR 83.07-99.73), respectively. Those with a past SARS-CoV-2 infection
had a higher median percent inhibition to BA.4/5 and WT, when compared
to uninfected individuals overall and when stratified by two or three
vaccine doses. When comparing two vs. three WT vaccine doses among
SARS-CoV-2 negative participants, we did not detect a difference in
BA.4/5 percent inhibition, but there was a difference in WT percent
inhibition. Among those with previous SARS-CoV-2 infection(s), when
comparing two vs. three WT vaccine doses, there was no observed
difference between groups. These findings demonstrate that additional
Whttps://www.covid19immunitytaskforce.ca/citf-databank/#accessing
<https://www.covid19immunitytaskforce.ca/citf-databank/#accessinguhan>
Hu-1 platform mRNA vaccines did not improve NtAb response to BA.4/5, but
prior SARS-CoV-2 infection enhances NtAb response.
</td>
<td style="text-align:left;">
The relationship between the number of COVID-19 vaccines and infection
with Omicron ACE2 inhibition at 18-months post initial vaccination in an
adult cohort of Canadian paramedics.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Electrophysiological neuromuscular alterations and severe fatigue
predict long-term muscle weakness in survivors of COVID-19 acute
respiratory distress syndrome.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Test negative design for vaccine effectiveness estimation in the context
of the COVID-19 pandemic: A systematic methodology review.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Pharmacists’ role and experiences with delivering mental health care
within team-based primary care settings during the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
The Covid-19 pandemic has prompted governments across the world to
enforce a range of public health interventions. We introduce the
Covid-19 Policy Response Canadian tracker (CPRCT) database that tracks
and records implemented public health measures in every province and
territory in Canada. The implementations are recorded on a four-level
ordinal scale (0-3) for three domains, (Schools, Work, and Other),
capturing differences in degree of response. The data-set allows the
exploration of the effects of public health mitigation on the spread of
Covid-19, as well as provides a near-real-time record in an accessible
format that is useful for a diverse range of modeling and research
questions.
</td>
<td style="text-align:left;">
Canadian Covid-19 pandemic public health mitigation measures at the
province level.
</td>
</tr>
<tr>
<td style="text-align:left;">
\[This corrects the article DOI: 10.2196/45767.\].
</td>
<td style="text-align:left;">
Figure Correction: Using Social Media to Help Understand
Patient-Reported Health Outcomes of Post-COVID-19 Condition: Natural
Language Processing Approach.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Worldwide scientific efforts on nursing in the field of SARS-CoV-2: a
cross-sectional survey analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Obesity and Outcomes of Kawasaki Disease and COVID-19-Related
Multisystem Inflammatory Syndrome in Children.
</td>
</tr>
<tr>
<td style="text-align:left;">
During the early years of the COVID-19 pandemic, preclinical and
clinical research were sped up and scaled up in both the public and
private sectors and in partnerships between them. This resulted in some
extraordinary advances, but it also raised a range of issues regarding
the ethics, rigour, and integrity of scientific research, academic
publication, and public communication. Many of the failures of
scientific rigour and integrity that occurred during the pandemic were
exacerbated by the rush to generate, disseminate, and implement research
findings, which not only created opportunities for unscrupulous actors
but also compromised the methodological, peer review, and advisory
processes that would usually identify sub-standard research and prevent
compromised clinical or policy-level decisions. While it would be
tempting to attribute these failures of science and its translation
solely to the “unprecedented” circumstances of the COVID-19 pandemic,
the reality is that they preceded the pandemic and will continue to
arise once it is over. Existing strategies for promoting scientific
rigour and integrity need to be made more rigorous, better integrated
into research training and institutional cultures, and made more
sophisticated. They might also need to be modified or supplemented with
other strategies that are fit for purpose not only in public health
emergencies but in any research that is sped-up and scaled up to address
urgent unmet medical needs.
</td>
<td style="text-align:left;">
The Fragility of Scientific Rigour and Integrity in “Sped up Science”:
Research Misconduct, Bias, and Hype and in the COVID-19 Pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
Worldwide, hypoxia-related conditions, including cancer, COVID-19, and
neuro-degenerative diseases, often lead to multi-organ failure and
significant mortality. Oxygen, crucial for cellular function, becomes
scarce as levels drop below 10 mmHg (&lt;2% O\<sub\>2\</sub\>),
triggering mitochondrial dysregulation and activating hypoxia-induced
factors (HiFs). Herein, oxygen nanobubbles (OnB), an \<i\>emerging\</i\>
versatile oxygen delivery platform, offer a novel approach to address
hypoxia-related pathologies. This review explores OnB oxygen delivery
strategies and systems, including diffusion, ultrasound, photodynamic,
and pH-responsive nanobubbles. It delves into the nanoscale mechanisms
of OnB, elucidating their role in mitochondrial metabolism (TFAM,
PGC1alpha), hypoxic responses (HiF-1alpha), and their interplay in
chronic pathologies including cancer and neurodegenerative disorders,
amongst others. By understanding these dynamics and underlying
mechanisms, this article aims to contribute to our accruing knowledge of
OnB and the developing potential in ameliorating hypoxia- and metabolic
stress-related conditions and fostering innovative therapies.
</td>
<td style="text-align:left;">
NanoBubble-Mediated Oxygenation: Elucidating the Underlying Molecular
Mechanisms in Hypoxia and Mitochondrial-Related Pathologies.
</td>
</tr>
<tr>
<td style="text-align:left;">
According to the Center for Disease Control and Prevention’s National
Breast and Cervical Cancer Early Detection Program, the cervical cancer
screening rate dropped by 84% soon after the declaration of the COVID-19
pandemic. The challenges facing cervical cancer screening were largely
attributed to the required in-person nature of the screening process and
the measures implemented to control the spread of the virus. While the
impact of the COVID-19 pandemic on cancer screening is well-documented
in high-income countries, less is known about the low- and middle-income
countries that bear 90% of the global burden of cervical cancer deaths.
In this paper, we aim to offer a comprehensive view of the impact of
COVID-19 on cervical cancer screening in LMICs. Using our study,
“Prevention of Cervical Cancer in India through Self-Sampling” (PCCIS),
as a case example, we present the challenges COVID-19 has exerted on
patients, healthcare practitioners, and health systems, as well as
potential opportunities to mitigate these challenges.
</td>
<td style="text-align:left;">
Surviving the Storm: The Impact of COVID-19 on Cervical Cancer Screening
in Low- and Middle-Income Countries.
</td>
</tr>
<tr>
<td style="text-align:left;">
This study aims to explore the differences in the psychological impact
of COVID-19 on physicians, specifically those who volunteered or were
contractually obligated to provide care for COVID-19 patients. While
previous research has predominantly focused on the physical health
consequences and risk of exposure for healthcare workers, limited
attention has been given to their work conditions. This sample comprised
300 physicians, with 68.0% of them men (mean age = 54.67 years; SD =
12.44; range: 23-73). Participants completed measurements including the
State-Trait Anxiety Inventory (STAI), Coping Inventory in Stressful
Situations (CISS), and Coronavirus Anxiety Scale (C.A.S.). Pearson’s
correlations were conducted to examine the relationships between the
variables of interest. This study employed multivariate models to test
the differences between work conditions: (a) involvement in COVID-19
patient care, (b) volunteering for COVID-19 patient management, (c)
contractual obligation to care for COVID-19 patients, and (d) COVID-19
contraction in the workplace. The results of the multivariate analysis
revealed that direct exposure to COVID-19 patients and contractual
obligation to care for them significantly predicted state anxiety and
dysfunctional coping strategies \[Wilks’ Lambda = 0.917 F = 3.254
\<i\>p\</i\> &lt; 0.001\]. In contrast, volunteering or being affected
by COVID-19 did not emerge as significant predictors for anxiety or
dysfunctional coping strategies. The findings emphasize the importance
of addressing the psychological well-being of physicians involved in
COVID-19 care and highlight the need for targeted interventions to
support their mental and occupational health.
</td>
<td style="text-align:left;">
Anxiety and Coping Strategies among Italian-Speaking Physicians: A
Comparative Analysis of the Contractually Obligated and Voluntary Care
of COVID-19 Patients.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Shared health governance, mutual collective accountability, and
transparency in COVAX: A qualitative study triangulating data from
document sampling and key informant interviews.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Disproportionate Sociodemographic Effects of Suspended Breast Cancer
Screening During the COVID-19 Pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Age and sex-related comparison of referral-based telemedicine service
utilization during the COVID-19 pandemic in Ontario: a retrospective
analysis.
</td>
</tr>
<tr>
<td style="text-align:left;">
While the COVID-19 pandemic has greatly exacerbated the mental health
challenges of transition-aged youth (TAY) between 17 and 29 years old,
it has also led to the rapid adoption of digital tools for mental health
help-seeking and treatment. However, to date, there has been limited
work focusing on how this shift has impacted perceptions, needs and
challenges of this population in using digital tools. The current study
aims to understand their perspectives on mental health help-seeking
during the pandemic and emerging issues related to digital tools (e.g.,
digital health equity, inclusivity). A total of 16 TAY were invited from
three post-secondary institutions in the Greater Toronto Area. A total
of two streams of focus groups were held and participants were invited
to share their perceptions, needs and experiences. Five main themes were
identified: 1) Helpfulness of a centralized resource encompassing a
variety of diverse mental health supports help-seeking; 2) The impact of
the shift to online mental health support on the use of informal
supports; 3) Digital tool affordability and availability; 4) Importance
of inclusivity for digital tools; and 5) Need for additional support for
mental health seeking and digital tool navigation. Future work should
examine how these needs can be addressed through new and existing
digital mental health help-seeking tools for TAY.
</td>
<td style="text-align:left;">
Enhancing the value of digital health tools for mental health
help-seeking in Canadian transitional aged youth during the pandemic:
Qualitative study.
</td>
</tr>
<tr>
<td style="text-align:left;">
Ionizable lipid nanoparticles (LNPs) pivotal to the success of COVID-19
mRNA (messenger RNA) vaccines hold substantial promise for expanding the
landscape of mRNA-based therapies. Nevertheless, the risk of mRNA
delivery to off-target tissues highlights the necessity for LNPs with
enhanced tissue selectivity. The intricate nature of biological systems
and inadequate knowledge of lipid structure-activity relationships
emphasize the significance of high-throughput methods to produce
chemically diverse lipid libraries for mRNA delivery screening. Here, we
introduce a streamlined approach for the rapid design and synthesis of
combinatorial libraries of biodegradable ionizable lipids. This led to
the identification of iso-A11B5C1, an ionizable lipid uniquely apt for
muscle-specific mRNA delivery. It manifested high transfection
efficiencies in muscle tissues, while significantly diminishing
off-targeting in organs like the liver and spleen. Moreover, iso-A11B5C1
also exhibited reduced mRNA transfection potency in lymph nodes and
antigen-presenting cells, prompting investigation into the influence of
direct immune cell transfection via LNPs on mRNA vaccine effectiveness.
In comparison with SM-102, while iso-A11B5C1’s limited immune
transfection attenuated its ability to elicit humoral immunity, it
remained highly effective in triggering cellular immune responses after
intramuscular administration, which is further corroborated by its
strong therapeutic performance as cancer vaccine in a melanoma model.
Collectively, our study not only enriches the high-throughput toolkit
for generating tissue-specific ionizable lipids but also encourages a
reassessment of prevailing paradigms in mRNA vaccine design. This study
encourages rethinking of mRNA vaccine design principles, suggesting that
achieving high immune cell transfection might not be the sole criterion
for developing effective mRNA vaccines.
</td>
<td style="text-align:left;">
Combinatorial design of ionizable lipid nanoparticles for
muscle-selective mRNA delivery with minimized off-target effects.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Postpandemic Evaluation of the Eco-Efficiency of Personal Protective
Equipment Against COVID-19 in Emergency Departments: Proposal for a
Mixed Methods Study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
A case of probable COVID-19 and mononucleosis reactivation complicating
the presentation of travel-acquired measles.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Randomized trial of the safety and efficacy of anti-SARS-CoV-2 mAb in
the treatment of patients with nosocomial COVID-19 (CATCO-NOS).
</td>
</tr>
<tr>
<td style="text-align:left;">
Canada experienced a wave of HPAI H5N1 outbreaks in the spring of 2022
with millions of wild and farmed birds being infected. Seabird
mortalities in Canada have been particularly severe on the Atlantic
Coast over the summer of 2022. Over 7 million birds have been culled in
Canada, and outbreaks continue to profoundly affect commercial bird
farms across the world. This new H5N1 virus can and has infected
multiple mammalian species, including skunks, foxes, bears, mink, seals,
porpoises, sea lions, and dolphins. Viruses with mammalian adaptations
such as the mutations PB2-E627K, E627V, and D701N were found in the
brain of various carnivores in Europe and Canada. To date this specific
clade of H5N1 virus has been identified in less than 10 humans. At the
ground level, awareness should be raised among frontline practitioners
most likely to encounter patients with HPAI.
</td>
<td style="text-align:left;">
Highly pathogenic avian influenza: Unprecedented outbreaks in Canadian
wildlife and domestic poultry.
</td>
</tr>
<tr>
<td style="text-align:left;">
While the WHO, public health experts, and political leaders have
referenced solidarity as an important part of our responses to COVID-19,
I consider how we build solidarity during pandemics in order to improve
the effectiveness of our responses. I use Prainsack and Buyx’s
definition of solidarity, which highlights three different tiers: (1)
interpersonal solidarity, (2) group solidarity, and (3) institutional
solidarity. Each tier of solidarity importantly depends on the actions
and norms established at the lower tiers. Although empathy and
solidarity are distinct moral concepts, I argue that the affective
component of solidarity is important for motivating solidaristic action,
and empathetic accounts of solidarity help us understand how we actually
build solidarity from tier to tier. During pandemics, public health
responses draw on different tiers of solidarity depending on the nature,
scope, and timeline of the pandemic. Therefore, I analyze both COVID-19
and HIV/AIDS using this framework to learn lessons about how solidarity
can more effectively contribute to our ongoing public health responses
during pandemics. Whereas we used institutional solidarity during
COVID-19 in a top-down approach to building solidarity that often
overlooked interpersonal and group solidarity, we used those lower tiers
during HIV/AIDS in a bottom-up approach because governments and public
health institutions were initially unresponsive to the crisis. Thus, we
need to ensure that we have a strong foundation of respect, trust, and
so forth, on which to build solidarity from tier to tier and promote
whichever tiers of solidarity are lacking during a given pandemic to
improve our responses.
</td>
<td style="text-align:left;">
Building solidarity during COVID-19 and HIV/AIDS.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Post-COVID-19 vaccination myocarditis: a prospective cohort study pre
and post vaccination using cardiovascular magnetic resonance.
</td>
</tr>
<tr>
<td style="text-align:left;">
Exposure to radiation oncology in medical school curricula is limited;
thus, mentorship and research opportunities like the Dr. Pamela Catton
Summer Studentship Program attempt to bridge this gap and stimulate
interest in the specialty. In 2021, the studentship was redesigned as
virtual research, mentorship, and case-based discussions due to the
COVID-19 pandemic. This study explores the impact of COVID-19 on the
studentship, on students’ perceptions of the program, and on medical
training and career choice. Fifteen studentship completion essays during
2021-2022 were obtained and anonymized. Thematic analysis was performed
to interpret the essays with NVivo. Two independent reviewers coded the
essays. Themes were established by identifying connections between coded
excerpts. Consensus was achieved through multiple rounds of discussion
and iteratively reviewing each theme. Representative quotes were used to
illustrate the themes. The themes confirmed the studentship was feasible
during the pandemic. Perceived benefits of the program included
mentorship and networking opportunities; gaining practical and
fundamental knowledge in radiation oncology; developing clinical and
research skills; and creating positive attitudes towards radiation
oncology and the humanistic aspect of the field. The studentship
supported medical specialty selection by helping define student values,
shaping perceptions of the specialty, and promoting self-reflection upon
students’ personal needs. This study informs future iterations of the
studentship to promote radiation oncology in Canadian medical school
curricula. It serves as a model for studentships in other specialties
that have limited exposure and similar challenges with medical student
recruitment.
</td>
<td style="text-align:left;">
A Qualitative Analysis of Medical Student Reflections Following
Participation in a Canadian Radiation Oncology Studentship.
</td>
</tr>
<tr>
<td style="text-align:left;">
Parosmia is a qualitative olfactory dysfunction characterized by
distortion of odor perception. Traditional treatments for parosmia
include olfactory training and steroids. Some patients infected with
COVID-19 have developed chronic parosmia as a result of their infection.
Here, we present the case of a patient who developed parosmia after a
COVID-19 infection that was not improved by traditional treatments but
found significant improvement after hyperbaric oxygen therapy\[A1\].
</td>
<td style="text-align:left;">
Hyperbaric oxygen therapy for treatment of COVID-19-related parosmia: a
case report.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Population-level effectiveness of pre-exposure prophylaxis for HIV
prevention among men who have sex with men in Montréal (Canada): a
modelling study of surveillance and survey data.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
COVID-19 in Long-Term Care: A Two-Part Commentary.
</td>
</tr>
<tr>
<td style="text-align:left;">
The COVID-19 pandemic resulted in a major shift in the delivery of
healthcare services with the adoption of care modalities to address the
diverse needs of patients. Besides, nurses, the largest profession in
the healthcare sector, were imposed with challenges caused by the
pandemic that influenced their intention to leave their profession. The
aim of the study was to examine the influence of mode of healthcare
delivery on nurses’ intention to quit job due to lack of satisfaction
during the pandemic in Canada. This cross-sectional study utilized data
from the Health Care Workers’ Experiences During the Pandemic (SHCWEP)
survey, conducted by Statistics Canada, that targeted healthcare workers
aged 18 and over who resided in the ten provinces of Canada during the
COVID-19 pandemic. The main outcome of the study was nurses’ intention
to quit within two years due to lack of job satisfaction. The mode of
healthcare delivery was categorized into; in-person, online, or blended.
Multivariable logistic regression was performed to examine the
association between mode of healthcare delivery and intention to quit
job after adjusting for sociodemographic, job-, and health-related
factors. Analysis for the present study was restricted to 3,430 nurses,
weighted to represent 353,980 Canadian nurses. Intention to quit job,
within the next two years, due to lack of satisfaction was reported by
16.4% of the nurses. Results showed that when compared to participants
who provided in-person healthcare services, those who delivered online
or blended healthcare services were at decreased odds of intention to
quit their job due to lack of job satisfaction (OR = 0.47, 95% CI:
0.43-0.50 and OR = 0.64, 95% CI: 0.61-0.67, respectively). Findings from
this study can inform interventions and policy reforms to address
nurses’ needs and provide organizational support to enhance their
retention and improve patient care during times of crisis.
</td>
<td style="text-align:left;">
Effect of mode of healthcare delivery on job satisfaction and intention
to quit among nurses in Canada during the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Investigating the Telerehabilitation with Aims to Improve Lower
Extremity Recovery Post-Stroke (TRAIL) Program: A Feasibility Study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The Implementation of a Virtual Emergency Department: Multimethods Study
Guided by the RE-AIM (Reach, Effectiveness, Adoption, Implementation,
and Maintenance) Framework.
</td>
</tr>
<tr>
<td style="text-align:left;">
Many COVID-19 survivors have post-COVID-19 conditions, and females are
at a higher risk. We sought to determine (1) how protein levels change
from acute to post-COVID-19 conditions, (2) whether females have a
plasma protein signature different from that of males, and (3) which
biological pathways are associated with COVID-19 when compared to
restrictive lung disease. We measured protein levels in 74 patients on
the day of admission and at 3 and 6 months after diagnosis. We
determined protein concentrations by multiple reaction monitoring (MRM)
using a panel of 269 heavy-labeled peptides. The predicted forced vital
capacity (FVC) and diffusing capacity of the lungs for carbon monoxide
(DLCO) were measured by routine pulmonary function testing. Proteins
associated with six key lipid-related pathways increased from admission
to 3 and 6 months; conversely, proteins related to innate immune
responses and vasoconstriction-related proteins decreased. Multiple
biological functions were regulated differentially between females and
males. Concentrations of eight proteins were associated with FVC, %, and
they together had \<i\>c\</i\>-statistics of 0.751 (CI:0.732-0.779);
similarly, concentrations of five proteins had \<i\>c\</i\>-statistics
of 0.707 (CI:0.676-0.737) for DLCO, %. Lipid biology may drive evolution
from acute to post-COVID-19 conditions, while activation of innate
immunity and vascular regulation pathways decreased over that period.
(ProteomeXchange identifiers: PXD041762, PXD029437).
</td>
<td style="text-align:left;">
Proteomic Evolution from Acute to Post-COVID-19 Conditions.
</td>
</tr>
<tr>
<td style="text-align:left;">
Food insecurity (FI) and water insecurity (WI) are linked with HIV
vulnerabilities, yet how these resource insecurities shape HIV
prevention needs is understudied. We assessed associations between FI
and WI and HIV vulnerabilities among urban refugee youth aged 16-24 in
Kampala, Uganda through individual in-depth interviews (IDI) (n = 24),
focus groups (n = 4), and a cross-sectional survey (n = 340) with
refugee youth, and IDI with key informants (n = 15). Quantitative data
was analysed via multivariable logistic and linear regression to assess
associations between FI and WI with: reduced pandemic sexual and
reproductive health (SRH) access; past 3-month transactional sex (TS);
unplanned pandemic pregnancy; condom self-efficacy; and sexual
relationship power (SRP). We applied thematic analytic approaches to
qualitative data. Among survey participants, FI and WI were commonplace
(65% and 47%, respectively) and significantly associated with: reduced
SRH access (WI: adjusted odds ratio \[aOR\]: 1.92, 95% confidence
interval \[CI\]: 1.19-3.08; FI: aOR: 2.31. 95%CI: 1.36-3.93), unplanned
pregnancy (WI: aOR: 2.77, 95%CI: 1.24-6.17; FI: aOR: 2.62, 95%CI:
1.03-6.66), and TS (WI: aOR: 3.09, 95%CI: 1.22-7.89; FI: aOR: 3.51,
95%CI: 1.15-10.73). WI participants reported lower condom self-efficacy
(adjusted β= -3.98, 95%CI: -5.41, -2.55) and lower SRP (adjusted β=
-2.58, 95%CI= -4.79, -0.37). Thematic analyses revealed: (1) contexts of
TS, including survival needs and pandemic impacts; (2) intersectional
HIV vulnerabilities; (3) reduced HIV prevention/care access; and (4)
water insecurity as a co-occurring socio-economic stressor. Multi-method
findings reveal FI and WI are linked with HIV vulnerabilities,
underscoring the need for HIV prevention to address co-occurring
resource insecurities with refugee youth.
</td>
<td style="text-align:left;">
HIV Vulnerabilities Associated with Water Insecurity, Food Insecurity,
and Other COVID-19 Impacts Among Urban Refugee Youth in Kampala, Uganda:
Multi-method Findings.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Co-development and evaluation of the Musculoskeletal Telehealth Toolkit
for physiotherapists.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Overcoming financial and social barriers during COVID-19: A medical
student-led medical education innovation.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The Effect of Telerehabilitation on Physical Fitness and
Depression/Anxiety in Post-COVID-19 Patients: A Randomized Controlled
Trial.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Zoomification of medical education: can the rapid online educational
responses to COVID-19 prepare us for another educational disruption? A
scoping review.
</td>
</tr>
<tr>
<td style="text-align:left;">
Long-term care homes (LTCHs) were disproportionately affected by the
coronavirus disease (COVID-19) pandemic, creating stressful
circumstances for LTCH employees, residents, and their care partners.
Team huddles may improve staff outcomes and enable a supportive climate.
Nurse practitioners (NPs) have a multifaceted role in LTCHs, including
facilitating implementation of new practices. Informed by a
community-based participatory approach to research, this mixed-methods
study aimed to develop and evaluate a toolkit for implementing NP-led
huddles in an LTCH. The toolkit consists of two sections. Section one
describes the huddles’ purpose and implementation strategies. Section
two contains six scripts to guide huddle discussions. Acceptability of
the intervention was evaluated using a quantitative measure (Treatment
Acceptability Questionnaire) and through qualitative interviews with
huddle participants. Descriptive statistics and manifest content
analysis were used to analyse quantitative and qualitative data. The
project team rated the toolkit as acceptable. Qualitative findings
provided evidence on design quality, limitations, and recommendations
for future huddles.
</td>
<td style="text-align:left;">
Development and Evaluation of a Nurse Practitioner Huddles Toolkit for
Long Term Care Homes.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Perspectives of Older Adults on COVID-19 and Influenza Vaccination in
Ontario, Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Humoral and T cell responses to SARS-CoV-2 reveal insights into immunity
during the early pandemic period in Pakistan.
</td>
</tr>
<tr>
<td style="text-align:left;">
Acute and chronic kidney disease continues to confer significant
morbidity and mortality in the clinical setting. Despite high prevalence
of these conditions, few validated biomarkers exist to predict kidney
dysfunction. In this study, we utilized a novel kidney multiplex panel
to measure 21 proteins in plasma and urine to characterize the spectrum
of biomarker profiles in kidney disease. Blood and urine samples were
obtained from age-/sex-matched healthy control subjects (HC),
critically-ill COVID-19 patients with acute kidney injury (AKI), and
patients with chronic or end-stage kidney disease (CKD/ESKD). Biomarkers
were measured with a kidney multiplex panel, and results analyzed with
conventional statistics and machine learning. Correlations were examined
between biomarkers and patient clinical and laboratory variables. Median
AKI subject age was 65.5 (IQR 58.5-73.0) and median CKD/ESKD age was
65.0 (IQR 50.0-71.5). Of the CKD/ESKD patients, 76.1% were on
hemodialysis, 14.3% of patients had kidney transplant, and 9.5% had CKD
without kidney replacement therapy. In plasma, 19 proteins were
significantly different in titer between the HC versus AKI versus
CKD/ESKD groups, while NAG and RBP4 were unchanged. TIMP-1 (PPV 1.0, NPV
1.0), best distinguished AKI from HC, and TFF3 (PPV 0.99, NPV 0.89) best
distinguished CKD/ESKD from HC. In urine, 18 proteins were significantly
different between groups except Calbindin, Osteopontin and TIMP-1.
Osteoactivin (PPV 0.95, NPV 0.95) best distinguished AKI from HC, and
β2-microglobulin (PPV 0.96, NPV 0.78) best distinguished CKD/ESKD from
HC. A variety of correlations were noted between patient variables and
either plasma or urine biomarkers. Using a novel kidney multiplex
biomarker panel, together with conventional statistics and machine
learning, we identified unique biomarker profiles in the plasma and
urine of patients with AKI and CKD/ESKD. We demonstrated correlations
between biomarker profiles and patient clinical variables. Our
exploratory study provides biomarker data for future hypothesis driven
research on kidney disease.
</td>
<td style="text-align:left;">
A novel multiplex biomarker panel for profiling human acute and chronic
kidney disease.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Implementing digital devices to maintain family connections during the
COVID-19 pandemic: Experience of a large academic urban hospital.
</td>
</tr>
<tr>
<td style="text-align:left;">
During the early phase of the COVID-19 pandemic, legislative changes
that expanded alcohol home delivery and options for “to-go” alcohol
sales were introduced across the United States to provide economic
relief to establishments and retailers. Using data from the Alcohol
Policy Information System, we examined whether these changes have
persisted beyond the peak phase of the COVID-19 emergency and explored
the implications for public health. Illustration of state-level policy
data reveals that the liberalisation of alcohol delivery and “to-go”
alcohol sales has continued throughout a 2-year period (2020 and 2021),
with indications that many of these changes have or will become
permanent after the pandemic. This raises concerns about inadequate
regulation, particularly in preventing underage access to alcohol, and
ensuing changes in drinking practices. In this commentary, we highlight
the need for rigorous empirical evaluation of the public health impact
of this changing policy landscape and underscore the potential risks
associated with increased alcohol availability, including a
corresponding increase in alcohol-attributable mortality and other
alcohol-related harm, such as domestic violence. Policy makers should
carefully consider public health consequences, whose costs may surpass
short-term economic interests in the long term.
</td>
<td style="text-align:left;">
Here to stay? Policy changes in alcohol home delivery and “to-go” sales
during and after COVID-19 in the United States.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
New causes of occupational allergic contact dermatitis.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Physical Rehabilitation Before and After Lung Transplantation for
COVID-19 ARDS: A Case Report.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Critical care delivery across health care systems in low-income and
low-middle-income country settings: A systematic review.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Variability in changes in physician outpatient antibiotic prescribing
from 2019 to 2021 during the COVID-19 pandemic in Ontario, Canada.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Enrollment of dengue patients in a prospective cohort study in Umphang
District, Thailand, during the COVID-19 pandemic: Implications for
research and policy.
</td>
</tr>
<tr>
<td style="text-align:left;">
Home care personal support service delivery decreased during the
COVID-19 pandemic, and qualitative studies have suggested many potential
contributors to these reductions. This paper provides insight into the
source (client or provider) of reductions in home care service volumes
early in the pandemic through analysis of a retrospective administrative
dataset from a large provider organization. The percentage of authorized
services not delivered was 17.2% in Wave 1, 12.6% in Wave 2 and 10.5% in
Wave 3, nearing the pre-pandemic baseline of 8.9%. The dominant
contribution to reduced home care service volumes was client-initiated
holds and cancellations, collectively accounting for 99.3% of the
service volume; missed care visits by the provider accounted for 0.7%.
Worker availability also declined due to long-term absences (which
increased 5-fold early in Wave 1 and remained 4× above baseline in Waves
2 and 3); short-term absences rose sharply for 6 early-pandemic weeks,
then dropped below the pre-pandemic baseline. These data reveal that
service volume reductions were primarily driven by client-initiated
holds and cancellations; despite unprecedented decreases in Personal
Support Worker availability, missed care did not increase, indicating
that the decrease in demand was more substantial and occurred earlier
than the decrease in worker availability.
</td>
<td style="text-align:left;">
Why Did Home Care Personal Support Service Volumes Drop During the
COVID-19 Pandemic? The Contributions of Client Choice and Personal
Support Worker Availability.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Overnight staffing in Canadian neonatal and pediatric intensive care
units.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of the COVID-19 pandemic on community-based brain injury
associations across Canada: a cross-sectional survey study.
</td>
</tr>
<tr>
<td style="text-align:left;">
Virtual care became a routine method for healthcare delivery during the
coronavirus disease 2019 (COVID-19) pandemic. Patient preferences are
central to delivering patient-centered and high-quality care. The
pandemic challenged healthcare organizations and providers to quickly
deliver safe healthcare to COVID-19 patients. This resulted in varied
implementation of virtual healthcare services. With an increased focus
on remote COVID-19 monitoring, little research has examined patient
experiences with virtual care. This scoping review examined patient
experiences and preferences with virtual care among community-based
self-isolating COVID-19 patients. We identified a paucity of literature
related to patient experiences and preferences regarding virtual care.
Few articles focused on patient experiences and preferences as a primary
outcome. Our research suggests that (1) patients view virtual care
positively and to be feasible to use; (2) patient access to technology
impacts patient satisfaction and experiences; and (3) to enhance the
patient experience, healthcare organizations and providers need to
support patient use of technology and resolve technology-related issues.
When planning virtual care modalities, purposeful consideration of
patient experiences and preferences is needed to deliver quality
patient-centered care.
</td>
<td style="text-align:left;">
What are COVID-19 Patient Preferences for and Experiences with Virtual
Care? Findings From a Scoping Review.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
New and continuing physician-based outpatient mental health care among
children and adolescents during the COVID-19 pandemic in Ontario,
Canada: a population-based study.
</td>
</tr>
<tr>
<td style="text-align:left;">
Since the onset of COVID-19, social protest has expanded significantly.
Little, however, has been written on prison-led and prison justice
organizing in the wake of the pandemic-particularly in the Canadian
context. This article is a case study of prisoner organizing in Canada
throughout the first 18 months of COVID-19, which draws on qualitative
interviews, media, and documentary analysis. We argue that the pandemic
generated conditions under which the grievances raised by prisoners, and
the strategies through which they were articulated, made possible a
discursive bridge to the anxieties and grievances experienced by those
in the community, thinning the walls of state-imposed societal
exclusion. We demonstrate that prisons are sites of fierce contestation
and are deeply embedded in, rather than separate from, our society. An
important lesson learned from this case study is the need for prison
organizing campaigns to strategically embrace multi-issue framing and
engage in sustained coalition building.
</td>
<td style="text-align:left;">
“A prison is no place for a pandemic”: Canadian prisoners’ collective
action in the time of COVID-19.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Opening the digital front door for individuals using long-term in-home
ventilation (LIVE) during a pandemic- implementation, feasibility and
acceptability.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Parents’ attitudes regarding their children’s play during COVID-19:
Impact of socioeconomic status and urbanicity.
</td>
</tr>
<tr>
<td style="text-align:left;">
In 2020, almost overnight, the paradigm for healthcare interactions
changed in Ontario. To limit person-to-person transmission of COVID-19,
the norm of in-person interactions shifted to virtual care. While this
shift was part of broader public health measures and an acknowledgment
of patient and societal concerns, it also represented a change in care
modalities that had the potential to affect the quality of care
provided, as well as short- and long-term patient outcomes. While public
policy decisions were being made to moderate the use of virtual care at
the end of the declared pandemic, a thorough analysis of short-term
patient outcomes was needed to quantify the impact of virtual care on
the population of Ontario.
</td>
<td style="text-align:left;">
Was Virtual Care as Safe as In-Person Care? Analyzing Patient Outcomes
at Seven and Thirty Days in Ontario during the COVID-19 Pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
Electronic health information that is easily accessible and shareable
among healthcare providers and their patients can provide substantial
improvements in Canada’s primary care system and population health
outcomes. The Commonwealth Fund’s (CMWF’s) 2022 International Health
Policy Survey of Primary Care Physicians (CIHI 2023) highlights the
views and experiences of primary care doctors in 10 developed countries,
including Canada. The survey covered various topics related to physician
workload, the use of information technology and coordination of care.
While the COVID-19 pandemic contributed to an increased physician
workload that may have impacted the ability to efficiently coordinate
care with other healthcare providers, Canadian family doctors did close
the gap with other countries as 93% of family doctors are now using
electronic medical records (EMRs) in their practices. The CMWF’s 2022
survey revealed challenges faced by Canadian family doctors in their
practices. However, international comparisons provide opportunities to
learn from other countries and build on the implementation of EMRs as
part of Canada’s shared health priorities.
</td>
<td style="text-align:left;">
The Commonwealth Fund Survey of Primary Care Physicians Reveals
Challenges Experienced by Family Doctors and Emphasizes the Need for
Interoperability of Health Information Technologies.
</td>
</tr>
<tr>
<td style="text-align:left;">
The COVID-19 pandemic has underscored the importance of addressing race
and ethnic disparities in healthcare worldwide. In Canada, however, the
lack of consistent capture of race and ethnicity data has hindered a
comprehensive understanding of these potential disparities. This article
explores the importance of and current progress in collecting race and
ethnic data in Canada and provides examples of its importance in
cardiovascular health outcomes. We believe that a successful
implementation of standardized data collection tools on race and
ethnicity data will shape evidence-based policies to minimize health
disparities in Canada in the future.
</td>
<td style="text-align:left;">
The Importance of Race and Ethnicity Data in Cardiovascular Health
Research.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Associations between changes in habitual sleep duration and lower
self-rated health among COVID-19 survivors: findings from a survey
across 16 countries/regions.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Utilization of physician mental health services by birthing parents with
young children during the COVID-19 pandemic: a population-based,
repeated cross-sectional study.
</td>
</tr>
<tr>
<td style="text-align:left;">
The Canadian health system is reeling following the COVID-19 pandemic.
Strains have become growing cracks, with long emergency department wait
times, shortage of human health resources, and growing dissatisfaction
from both clinicians and patients. To address long needed health system
reform in Canada, a modernization of training is required for the next
generation health leaders. The Canadian Institutes of Health Research
Health System Impact Fellowship is an example of a well-funded and
connected training program which prioritizes embedded research and
embedding technically trained scholars with health system partners. The
program has been successful in the scope and impact of its training
outcomes as well as providing health system partners with a pool of
connected and capable scholars. Looking forward, integrating aspects of
evidence synthesis from both domestic and international sources and
adapting a general contractor approach to implementation within the HSIF
could help catalyze Learning Health System reform in Canada.
</td>
<td style="text-align:left;">
Future leaders in a Learning Health System: Exploring the Health System
Impact Fellowship.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
SARS-CoV-2 Variants Omicron BA.4/5 and XBB.1.5 Significantly Escape T
Cell Recognition in Solid-organ Transplant Recipients Vaccinated Against
the Ancestral Strain.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of the coronavirus disease 2019 pandemic on equity of access to
hip and knee replacements: a population-level study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Practice Facilitation to Support Family Physicians in Encouraging
COVID-19 Vaccine Uptake: A Multimethod Process Evaluation.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
The successful and safe conversion of joint arthroplasty to same-day
surgery: A necessity after the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
A multimethods randomized trial found that plain language versions
improved adults understanding of health recommendations.
</td>
</tr>
<tr>
<td style="text-align:left;">
Severe acute respiratory syndrome coronavirus-2 (SARS-CoV-2) causes
coronavirus disease 2019 (COVID-19), which has killed ~7 million persons
worldwide. Chronic kidney disease (CKD) is the most common risk factor
for severe COVID-19 and one that most increases the risk of
COVID-19-related death. Moreover, CKD increases the risk of acute kidney
injury (AKI), and COVID-19 patients with AKI are at an increased risk of
death. However, the molecular basis underlying this risk has not been
well characterized. CKD patients are at increased risk of death from
multiple infections, to which immune deficiency in non-specific host
defenses may contribute. However, COVID-19-associated AKI has specific
molecular features and CKD modulates the local (kidney) and systemic
(lung, aorta) expression of host genes encoding coronavirus-associated
receptors and factors (SCARFs), which SARS-CoV-2 hijacks to enter cells
and replicate. We review the interaction between kidney disease and
COVID-19, including the over 200 host genes that may influence the
severity of COVID-19, and provide evidence suggesting that kidney
disease may modulate the expression of SCARF genes and other key host
genes involved in an effective adaptive defense against coronaviruses.
Given the poor response of certain CKD populations (e.g., kidney
transplant recipients) to SARS-CoV-2 vaccines and their suboptimal
outcomes when infected, we propose a research agenda focusing on CKD to
develop the concept of comorbidity-specific targeted therapeutic
approaches to SARS-CoV-2 infection or to future coronavirus infections.
</td>
<td style="text-align:left;">
SCARF Genes in COVID-19 and Kidney Disease: A Path to
Comorbidity-Specific Therapies.
</td>
</tr>
<tr>
<td style="text-align:left;">
In the present study, we aimed to assess the frequency of and the
relationships between alexithymia, burnout, and hopelessness in a large
sample of healthcare workers (HCWs) during the third wave of COVID-19 in
Italy. Alexithymia was evaluated by the Italian version of the 20-item
Toronto Alexithymia Scale (TAS-20) and its subscales Difficulty in
Identifying Feelings (DIF), Difficulty in Describing Feelings (DDF), and
Externally Oriented Thinking (EOT), burnout was measured with the scales
emotional exhaustion (EE), depersonalisation (DP), and personal
accomplishment (PA) of the Maslach Burnout Test (MBI), hopelessness was
measured using the Beck Hopelessness Scale (BHS), and irritability
(IRR), depression (DEP), and anxiety (ANX) were evaluated with the
Italian version of the Irritability’ Depression’ Anxiety Scale (IDA).
This cross-sectional study recruited a sample of 1445 HCWs from a large
urban healthcare facility in Italy from 1 May to 31 June 2021. The
comparison between individuals that were positive (\<i\>n\</i\> = 214,
14.8%) or not for alexithymia (\<i\>n\</i\> = 1231, 85.2%), controlling
for age, gender, and working seniority, revealed that positive subjects
showed higher scores on BHS, EE, DP IRR, DEP, ANX, DIF, DDF, and EOT and
lower on PA than the not positive ones (\<i\>p\</i\> &lt; 0.001). In the
linear regression model, higher working seniority as well as higher EE,
IRR, DEP, ANX, and DDF scores and lower PA were associated with higher
hopelessness. In conclusion, increased hopelessness was associated with
higher burnout and alexithymia. Comprehensive strategies should be
implemented to support HCWs’ mental health and mitigate the negative
consequences of alexithymia, burnout, and hopelessness.
</td>
<td style="text-align:left;">
Alexithymia, Burnout, and Hopelessness in a Large Sample of Healthcare
Workers during the Third Wave of COVID-19 in Italy.
</td>
</tr>
<tr>
<td style="text-align:left;">
To assess the impact of the COVID-19 pandemic on the diagnosis, staging
and outcome of a selected population throughout the first two years of
the pandemic, we evaluated oncology patients undergoing PET/CT at our
institution. A retrospective population of lung cancer, melanoma,
lymphoma and head and neck cancer patients staged using PET/CT during
the first 6 months of the years 2019, 2020 and 2021 were included for
analysis. The year in which the PET was performed was our exposure
variable, and our two main outcomes were stage at the time of the PET/CT
and overall survival (OS). A total of 1572 PET/CTs were performed for
staging purposes during the first 6 months of 2019, 2020 and 2021. The
median age was 66 (IQR 16), and 915 (58%) were males. The most prevalent
staged cancer was lung cancer (643, 41%). The univariate analysis of
staging at PET/CT and OS by year of PET/CT were not significantly
different. The multivariate Cox regression of non-COVID-19 significantly
different variables at univariate analysis and the year of PET/CT
determined that lung cancer (HR 1.76 CI95 1.23-2.53, \<i\>p\</i\> &lt;
0.05), stage III (HR 3.63 CI95 2.21-5.98, \<i\>p\</i\> &lt; 0.05), stage
IV (HR 11.06 CI95 7.04-17.36, \<i\>p\</i\> &lt; 0.05) and age at
diagnosis (HR 1.04 CI95 1.02-1.05, \<i\>p\</i\> &lt; 0.05) had increased
risks of death. We did not find significantly higher stages or reduced
OS when assessing the year PET/CT was performed. Furthermore, OS was not
significantly modified by the year patients were staged, even when
controlled for non-COVID-19 significant variables (age, type of cancer,
stage and gender).
</td>
<td style="text-align:left;">
Impact of the COVID-19 Pandemic on Staging Oncologic PET/CT Imaging and
Patient Outcome in a Public Healthcare Context: Overview and Follow Up
of the First Two Years of the Pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Corruption risks in health procurement during the COVID-19 pandemic and
anti-corruption, transparency and accountability (ACTA) mechanisms to
reduce these risks: a rapid review.
</td>
</tr>
<tr>
<td style="text-align:left;">
As digital technologies continue to impact medicine, emergency medicine
providers have an opportunity to work together to harness these
technologies and shape their implementation within our healthcare
system. COVID-19 and the rapid scaling of virtual care provide an
example of how profoundly emergency medicine can be affected by digital
technology, both positively and negatively. This example also
strengthens the case for why EM providers can help lead the integration
of digital technologies within our broader healthcare system. As virtual
care becomes a permanent fixture of our system, and other technologies
such as AI and wearables break into Canadian healthcare, more advocacy,
research, and health system leadership will be required to best leverage
these tools. This paper outlines the purpose and outputs of the newly
founded CAEP Digital Emergency Medicine (DigEM) Committee, with the hope
of inspiring further interest amongst CAEP members and creating
opportunities to collaborate with other organizations within CAEP and
across EM groups nationwide.
</td>
<td style="text-align:left;">
Implementing digital emergency medicine: a call to action.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Protection conferred by COVID-19 vaccination, prior SARS-CoV-2
infection, or hybrid immunity against Omicron-associated severe outcomes
among community-dwelling adults.
</td>
</tr>
<tr>
<td style="text-align:left;">
The provision of clinical neuropsychological services has predominately
been undertaken by way of standardized administration in a face-to-face
setting. Interpretation of psychometric findings in this context is
dependent on the use of normative comparison. When the standardization
in which such psychometric measures are employed deviates from how they
were employed in the context of the development of its associated norms,
one is left to question the reliability and hence, validity of any such
findings and in turn, diagnostic decision making. In light of the
current COVID-19 pandemic and resultant social distancing direction,
face-to-face neuropsychological assessment has been challenging to
undertake. As such, remote (i.e., virtual) neuropsychological assessment
has become an obvious solution. Here, and before the results from remote
neuropsychological assessment can be said to stand on firm scientific
grounds, it is paramount to ensure that results garnered remotely are
reliable and valid. To this end, we undertook a review of the literature
and present an overview of the landscape. To date, the literature shows
evidence for the reliability of remote administration and the clinical
implications are paramount. When and where needed, neuropsychologists,
psychometric technicians and examinees may no longer need to be in the
same physical space to undergo an assessment. These findings are most
relevant given the physical distancing practices because of COVID-19.
And whilst remote assessment should never supplant face-to-face
neuropsychological assessments, it does serve as a valid alternative
when necessary.
</td>
<td style="text-align:left;">
A review of the reliability of remote neuropsychological assessment.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
A qualitative analysis of gestational surrogates’ healthcare experiences
during the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Efficacy of an Electronic Cognitive Behavioral Therapy Program Delivered
via the Online Psychotherapy Tool for Depression and Anxiety Related to
the COVID-19 Pandemic: Pre-Post Pilot Study.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Parental mental health trajectories over the COVID-19 pandemic and links
with childhood adversity and pandemic stress.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Impact of the COVID-19 pandemic on Canadian emergency medical system
management of out-of-hospital cardiac arrest: A retrospective cohort
study.
</td>
</tr>
<tr>
<td style="text-align:left;">
COVID-19 continues to be a global health concern and booster doses are
necessary for maintaining vaccine-mediated protection, limiting the
spread of SARS-CoV-2. Despite multiple COVID vaccine options, global
booster uptake remains low. Reactogenicity, the occurrence of adverse
local/systemic side effects, plays a crucial role in vaccine uptake and
acceptance, particularly for booster doses. We conducted a targeted
review of the reactogenicity of authorized/approved mRNA and
protein-based vaccines demonstrated by clinical trials and real-world
evidence. It was found that mRNA-based boosters show a higher incidence
and an increased severity of reactogenicity compared with the Novavax
protein-based COVID vaccine, NVX-CoV2373. In a recent NIAID study, the
incidence of pain/tenderness, swelling, erythema, fatigue/malaise,
headache, muscle pain, or fever was higher in individuals boosted with
BNT162b2 (0.4 to 41.6% absolute increase) or mRNA-1273 (5.5 to 55.0%
absolute increase) compared with NVX-CoV2373. Evidence suggests that
NVX-CoV2373, when utilized as a heterologous booster, demonstrates less
reactogenicity compared with mRNA vaccines, which, if communicated to
hesitant individuals, may strengthen booster uptake rates worldwide.
</td>
<td style="text-align:left;">
The Novavax Heterologous COVID Booster Demonstrates Lower Reactogenicity
Than mRNA: A Targeted Review.
</td>
</tr>
<tr>
<td style="text-align:left;">
Roughly 25% of the RTs in our study were considering leaving their
position in the spring of 2021. Compared to RTs not considering leaving
their position, those considering leaving reported elevated moral
distress, functional impairment and adverse psychological outcomes.
Previous consideration of leaving one’s position, having left a position
in the past, system-level moral distress and PTSD symptoms significantly
increased the odds of considering leaving; however, the contribution of
system-level moral distress and PTSD symptoms were each small. Broader,
organizational issues may play an additional role in consideration of
position departure among Canadian RTs, and is an area for future
research.
</td>
<td style="text-align:left;">
Canadian respiratory therapists who considered leaving their clinical
position experienced elevated moral distress and adverse psychological
and functional outcomes during the COVID-19 pandemic.
</td>
</tr>
<tr>
<td style="text-align:left;">
RéSUMé: OBJECTIF: La transmission de la COVID-19, l’émergence de
variants préoccupants et l’affaiblissement de l’immunité ont conduit à
recommander des doses de rappel de vaccin contre la COVID-19.
L’hésitation à la vaccination remet en question une large couverture
vaccinale. Nous avons déployé une enquête transnationale pour étudier
les connaissances, les croyances et les comportements en faveur de la
poursuite de la vaccination contre la COVID-19. MéTHODES: Nous avons
mené une enquête nationale transversale en ligne auprès d’adultes au
Canada, entre le 16 et le 26 mars 2022. Nous avons utilisé des
statistiques descriptives pour résumer notre échantillon et testé les
différences démographiques, les perceptions de l’efficacité des vaccins,
les doses recommandées et la confiance dans les décisions, en utilisant
la correction de Rao-Scott pour les tests du chi carré pondérés. La
régression logistique multivariée a été ajustée pour les covariables
pertinentes afin d’identifier les facteurs sociodémographiques et les
croyances associés à l’hésitation à la vaccination. RéSULTATS: Nous
avons collecté 2 202 questionnaires remplis. Un faible niveau
d’éducation (lycée : rapport de cotes (OR) 1,90, intervalle de confiance
(IC) à 95% 1,29, 2,81) et le fait d’avoir des enfants (OR 1,89, IC 1,39,
2,57) étaient associés à une probabilité accrue d’éprouver une
hésitation à l’égard d’une dose de rappel, tandis qu’un revenu plus
élevé (100 000 \$–149 999 \$ : OR 0,60, IC 0,39, 0,91; 150 000 \$ ou
plus : OR 0,49, IC 0,29, 0,82) était associé à une diminution des
probabilités. Incrédulité dans l’efficacité du vaccin (contre
l’infection : OR 3,69, IC 1,98, 6,90; maladie grave : OR 3,15, IC 1,69,
5,86), en désaccord avec la prise de décision du gouvernement (plutôt en
désaccord : OR 2,70, IC 1,38, 5,29; fortement en désaccord : OR 4,62, IC
2,20, 9,7) et la croyance dans le sur-vaccination (OR 2,07, IC 1,53,
2,80) ont été associées à une hésitation à recevoir une dose de rappel.
CONCLUSION: Une hésitation à l’égard du vaccin contre la COVID-19 peut
se développer ou augmenter à l’égard des vaccins ultérieurs. Nos
résultats indiquent des facteurs à prendre en compte lors du ciblage des
populations hésitantes à la vaccination.
</td>
<td style="text-align:left;">
Factors affecting hesitancy toward COVID-19 vaccine booster doses in
Canada: a cross-national survey.
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
SARS-CoV-2 vaccination prevalence by mental health diagnosis: a
population-based cross-sectional study in Ontario, Canada.
</td>
</tr>
</tbody>
</table>

</div>

``` r
# The table will likely be huge. How can we make the output look better?
# (one idea: kableExtra::scroll_box())
```

Done! Knit the document, commit, and push.

## Final Pro Tip (optional)

You can still share the HTML document on github. You can include a link
in your `README.md` file as the following:

``` md
View [here](https://cdn.jsdelivr.net/gh/:user/:repo@:tag/:file) 
```

For example, if we wanted to add a direct link the HTML page of lecture
6, we could do something like the following:

``` md
View Week 6 Lecture [here]()
```
