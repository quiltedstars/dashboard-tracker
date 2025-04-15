---
title: "writeup"
output: html_document
---

### Megan Krempa

## Problem Statement

The original intention of this project was to explore the connections between author, country, and date of a library collection (originally the British Museum's library). I wanted to visualize how colonialism impacts the collections and items that are or were historically held. However, attempting to do OCR proved beyond my willingness to troubleshoot, so instead I turned towards looking at the Wellcome collection and the Smithsonian collections. In the end, I ended up using the Smithsonian collections, namely the Smithsonian Institution Libraries open datasets available through AWS buckets.

My question remained the same–how does their collection reflect the history of collecting and colonialism? Suddenly, though, Trump began to threaten institutions like the Smithsonian, and so I became interested in figuring out a way to compare a past downloaded set of data to the data that is updated weekly on their AWS buckets.

My primary question that I sought to answer, then, was really: [*What items are being removed from the Smithsonian open data datasets*]{.underline}*?* And following from that, what the items may say about the way in which the Smithsonian is handling Trump's orders.

## Methods

I decided to do two types of analysis. The first was figuring out how to create a reactive choropleth map for items which had countries and dates mentioned. The second was figuring out the most efficient way to compare two massive data files. These two things took me a significant amount of time.

The first issue I encountered was that the Smithsonian datasets were, as I would eventually come to realize, in line-delimited JSON format. This meant that I spent about a week trying to figure out a Python script that would convert the files into regular JSON, because using jsonlite didn't work. In fact, it was only in the last day I realized I could've saved myself a thousand hours when I realized that ndjson would automatically unnest my data–another major headache.

All that being said, the following code is what I executed in order to get manageable and relatively clean data that did not crash my computer by running out of memory (something I've never managed to do before this assignment!):

```         
# In terminal, need to install the aws package first
aws s3 cp --no-sign-request s3://smithsonian-open-access/metadata/edan/sil/ . --recursive 
```

This downloads the entire AWS directory into the directory you are in. I had done this in March.

Because this is a large directory, trying to cat the files into one and loading it into R makes things crash. So, after a LOT of trial and error, I used this code to pull only the most important data.

I cat'd(?) the txt files into one large file (app3 because I had several folders made from trial and error already):

```         
cat *.txt > app3.json
```

Again, originally I kept trying to unnest the whole json into R, which was just unmanageable for anything larger than a few thousand lines. Using the first file downloaded, I inspected the file to find the parts I wanted. I then used the following jq code to get a much more manageable file, id_content.json

``` jq
jq '{id: .id, url: .content.descriptiveNonRepeating.record_link, notes: .content.freetext.notes[0].content, description: .content.freetext.physicalDescription[0].content, date: .content.freetext.date[0].content, name: .content.freetext.name[0].content, title: .content.descriptiveNonRepeating.title.content, place: .content.indexedStructured.geoLocation[0].L2}' app3.json | jq -c > id_content.json
```

In R, I then found out that ndjson did something interesting, in that it made two columns for the place, which I realized was because at times the data was such that geolocation[0].L2 sometimes had one value (just content), but normally had two (type and content). As such, here was the code to then create a csv, so that the Shiny app doesn't need to keep rendering the json (which takes a long time):

``` r
txt <- ndjson::stream_in("../id_content.json")
txt <- txt %>% select(-10) 
txt2 <- txt %>% mutate(
  place = coalesce(place, place.content)
)

write_csv(txt2, "olddata.csv")
```

Then, I got to learn about getting aws to work recursively and within R:

``` r
Sys.setenv(
  "AWS_DEFAULT_REGION" = "us-west-2"
)

get_bucket(object, max = Inf)
bucket <- "smithsonian-open-access"
prefix <- "metadata/edan/sil/"
desired_cols <- c("id", "title")

files <- get_bucket(bucket = bucket, prefix = prefix)

keys <- sapply(files, function(x) x[["Key"]])

all_data <- lapply(keys, function(key) {
  message("Reading: ", key)
  tryCatch({
    s3read_using(
      FUN = function(con) {
        stream_in(con) %>% 
          select(any_of(desired_cols))
        },
      object = key,
      bucket = bucket)
  }, error = function(e) {
    warning("Failed to read ", key)
    NULL
  })
})

write_csv(combined, "all_data.csv")
```

I had been familiar with aws when I was experimenting on the command line before. However, while it was straightforward to get the entire files and their contents, it once again tended to crash my computer, so I needed to figure out how to get it to filter only the columns I wanted. For this new data, because I was only using it to compare against the old data, I decided the best method would be using the id (meant to be a unique key), and the title (to function as a check).

Having title included actually made a big difference when looking at the comparisons. For example, you may notice there is a book called "Shade: a tale of two presidents" which no longer showed up in the data. I thought to myself: *I wonder if this book was about Trump?* Following the url of the original item, the page no longer exists. But if you search on the Smithsonian site, it does still seem like it shows up, but not at the url and nowhere else in the new data, implying some sort of disconnect between the datasets and the backend/frontend of the actual Smithsonian website. You can find the current page here: [Shade: a tale of two presidents](https://siris-libraries.si.edu/ipac20/ipac.jsp?session=174464G6W5Y90.72416&menu=search&aspect=Keyword&npp=20&ipp=20&spp=20&profile=liball&ri=3&source=%7E%21silibraries&index=GW&term=shade+a+tale+of+two+presidents&x=0&y=0&aspect=Keyword#focus)

#### Back to the code itself

Going back to the actual data that ended up being included, there are the following variables from the dataset:

| variable | context |
|----|----|
| date | Meant to be the first/most prominent/important date. However, a lot of the data is malformed (with letters to represent vague dates), or sometimes even having weird structures (like datetime structure) |
| description | The first description of the item |
| id | The unique id of each item |
| name | The first author/contributor/artist of the item |
| notes | The first notes for the item |
| place | The first country location indicated for the item |
| title | The title of the item |
| url | The url of the item (linked to the Smithsonian site) |
| place.content | A remnant of improper json and data, it had country names that I then combined into the original place. |

There was a tenth column (indicated in the previous R code) which was place.type–normally the place should have a structure of type: country (or continent or state) and then content: (name), but there were items where it only had content. So I dropped the place.type since the only unique value was Country.

For the choropleth map, the geojson file was found here: [NE Geojson from nvkelso (Github)](https://github.com/nvkelso/natural-earth-vector/blob/master/geojson/ne_50m_admin_0_countries.geojson)

I will also say that a lot of my code necessarily came from knowledge I learned from sources online. I also used ChaptGPT to assist in code issues I could not find solutions to online. I feel as though using AI, though, has made me much more aware of the fact it is not intelligent! Many times I found my own code/attempts were more intuitive and clear than anything GPT produced.

The following were some of the sources I found useful to solve some of my issues. Not included are the documentation sites for packages themselves.

[Stack Overflow for when I was trying to unnest json](https://stackoverflow.com/questions/41180960/convert-nested-json-to-csv-file-in-python)

[Stack Overflow - How to dynamically/reactively render choropleth maps](https://stackoverflow.com/questions/53480924/dynamically-render-choropleth-map-with-sliderinput-in-r-shiny/53482005#53482005)

[Geeksforgeeks - Trying to figure out looping files](https://www.geeksforgeeks.org/how-to-read-many-files-in-r-with-loop/)

[Stack Overflow - Getting rmarkdown to integrate into Shiny!](https://stackoverflow.com/questions/33499651/rmarkdown-in-shiny-application)

[Shiny Dashboard help](https://tilburgsciencehub.com/topics/visualization/data-visualization/dashboarding/shinydashboard/)

[RStudio shinydashboard](https://rstudio.github.io/shinydashboard/structure.html)

## Analysis

Looking at the choropleth map, we can see that, perhaps unsurprisingly, the United States is the overwhelming origin for items. However, some of the next most popular countries include China, Japan, Nigeria, South Africa, Canada, India, Egypt, and Peru. As this is a reflection of the library collections, it does not include works or artefacts that are more likely to have been taken from other countries through colonialism or questionable provenance. However, I think the fact that the second most popular location after the US is China says a lot about the ways in which colonialism has connections to collections. I also think it is interesting considering US-China relations.

That being said, a significant amount of items have been collected in the past 25 years (1999-2024(, with 15,000 items from the US and 4200 from China alone.

Going further back, from the earliest range up to 1800, we see, unsurprisingly, that the UK, US, Italy, and France are at the top. But what I think is far more interested is in the places with few items. For example, there are items from The Gambia, Senegal, Syria, Sudan, Papua New Guinea, and even Kyrgyzstan. Looking at these items, we can see a very colonial history emerge. Taking Palau, which between 1,008 and 1,801 has five items in the collection, we see the results as follows:

| Title | Name | Date | Description | Notes |
|----|----|----|----|----|
| Narrative of the shipwreck of the Antelope East-India pacquet on the Pelew Islands : situated on the western part of the Pacific Ocean, in August, 1783 | Bayer, Frederick M. DSI | 1788 | [2], viii, 268 p., [2] leaves of plates : ill. ; 18 cm | Signatures: pi1 a⁴ A-Y⁶ Z². |
| An account of the Pelew islands, situated in the western part of the Pacific ocean. Composed from the journals and communications of Captain Henry Wilson, and some of his officers, who, in August 1783, were there shipwrecked, in the Antelope ... by George Keate .. | Keate, George 1729-1797 | 1788 | xxvii, [1], 378 p. front., plates, ports., fold. map, plan. 31 cm | "A vocabulary of the Pelew language": p. 365-378. |
| The history of Prince Lee Boo : a native of the Pelew Islands brought to England by Captn. Wilson .. | Wilson, Henry d. 1810 | 1790 | viii, 178 p., [4] leaves of plates : ill. ; 14 cm. (18mo) | Engraved t.p. and title vignette. |
| An account of the Pelew Islands : situated in the western part of the Pacific Ocean : composed from the journals and communications of Captain Henry Wilson and some of his officers, who, in August 1783, were there shipwrecked in the Antelope, a packet belonging to the honourable East India Company / by George Keate .. | Keate, George 1729-1797 | 1788 | xxvii, [i], 378, [2] p., [17] leaves of plates (2 folded) : ill. (etchings), map, ports. ; 30 cm. (4to) | "A vocabulary of the Pelew language": p. 365-378. |
| The history of Prince Lee Boo : a native of the Pelew Islands : brought to England by Captn. Wilson | Newbery, E (Elizabeth) 1746-1821 | 1789 | [iii]-vi, 178, [6] p., [7] leaves of plates : ill., port. (wood engravings) ; 14 cm. (18mo) | There is only one undated edition printed for E. Newbery with the above collation in Roscoe. This is assigned the number J170(2). Roscoe does admit that there are many undated copies of this title that he has not examined. |

What we see here is that while there are some duplicates, every item is about a colonial endeavour and/or slavery. We see this reflected in other countries as well, like Peru. While these are not priceless artifacts taken by colonial expeditions, what this data does show are tangible examples of the loss of information. Peru, for example, shows several copies of Garcilaso de la Vega's "The royal commentaries of Peru", whose testimony showed that there were existing languages and written works that were destroyed by the colonial empire.

Speaking to the items that have been removed from the datasets, there are several that I find particularly interesting. I have mentioned already *Shade: a tale of two presidents*. I find it interesting that the work no longer is included in the datasets even though it still exists on the Smithsonian site.

Three other items spark my interest. First, the Grants awarded/Getty Grant Program, possibly from 1992 or 1993. I would be curious to know what was in the program, and who was given grants for that year.

The more concerning, to me, is in the New Museum news and the New Museum Papers (the latter's title being just "Paper"). In the notes to Paper it says that "New Museum Paper is a bi-annual newsletter containing information on institutional and educational activities, new initiatives, upcoming and current exhibitions, events, public programs and publications". Now, perhaps over the past week (as the AWS is meant to update weekly) employees have either temporarily removed these items from the datasets in order to clean them up better. But it is certainly striking that works which are about education and a work about Donald Trump being compared to Barack Obama have been removed.

## Conclusion

In the midst of doing this project, I was talking to a professor over at King's (Melanie Frappier) and we were discussing doing 'good trouble' and trying to figure out how to fight back against the mandates being imposed in the US. That's when I had the idea about the comparison. I believe this work can be further extended.

After the conversation, I started looking into the Master of Journalism program at King's, which has a large emphasis on data journalism and investigative journalism. I hope to be able to use this work as a springboard to a more permanent (and visible) site.

In conclusion, the datasets provided by the Smithsonian Institution Libraries provide a clear way to model and interpret collections. Several things could be added, for example: word clouds based on the item topic(s), an integrated choropleth map working with the removed items, more localized locations, etc.

For now, though, I shall end it here.
