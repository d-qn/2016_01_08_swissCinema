library("dplyr")
library("swiTheme")

topMovies <- 25
height <- 700

data.file <- "input/Classement 25 blockbusters suisses (en Suisse) - swiss_blockbusters_data.csv"
translation.file <- "input/Classement 25 blockbusters suisses (en Suisse) - swiss_blockbusters_translations.csv"

data.read <- read.csv(data.file, stringsAsFactors = F, row.names = 1, check.names = F)
data.read$Entrées <- as.numeric(gsub(",", "", data.read$Entrées))
txt <- read.csv(translation.file, stringsAsFactors = F, row.names = 1, check.names = F)


## Interactive bubble chart
library(swiRcharts)

data <- data.read %>% select(Titre, `Réalisateur / Réalisatrice`,
  `Genre_2`, Langue, Année, Entrées, IMDB, Affiche_pic_url, Trailer) %>%
  rename(x = Année, y = Entrées, z = IMDB, series = Genre_2, url = Trailer)

# order data by entry number and subset data & translastion
data <- data[order(data$y, decreasing = T),]
data <- data[1:topMovies, ]

idx <- which(grepl( "synopsis$", rownames(txt)))
toDelete <- !rownames(txt)[idx] %in% paste0(rownames(data), ".synopsis")
txt <- txt[-idx[toDelete],]

#for(lang in colnames(txt)[nchar(colnames(txt))==2]) {
  lang <- 'FR'
  dd <- data
  dd$title <- txt[rownames(data),lang]
  dd$series <- txt[paste0(tolower(gsub(" |\\/", "", dd$series)), ".type"), lang]

  ## create fancy tooltip as html table
  dd$name <- paste0(
    '<table cellpadding="1" style="line-height:1.2">',
    '<tr><td colspan="3"><b>', dd$title, '</b></td></tr>',
    '<tr><td colspan="3"><i>', dd$Titre, '</i></td></tr></div>',
    '<tr><td colspan="3"><img src="', dd$Affiche_pic_url, '" width="220"></td></tr>',
    '<tr><td>', txt['director.cat', lang], '</td><td colspan = 2>', dd$`Réalisateur / Réalisatrice`, '</td></tr>',
    '<tr><td>', txt['admissions.cat', lang], '</td><td colspan = 2>', dd$y, '</td></tr>',
    '<tr><td>', txt['year.cat', lang], '</td><td colspan = 2>', dd$x, '</td></tr>',
    '<tr><td>', txt['imdb.cat', lang], '</td><td colspan = 2>', dd$z, ' / 10</td></tr>',
    '<tr><td>', txt['language.cat', lang], '</td><td colspan = 2>',
      ifelse(dd$Langue == 'GER', txt['ger.type', lang], txt['fre.type', lang]), '</td></tr>',
    '<tr><td>', txt['genre.cat', lang], '</td><td colspan = 2>', dd$series, '</td></tr>',
    '<tr><td colspan="3"> </td></tr></div>',
    '<tr><td colspan="3">', sapply(strwrap(gsub("('|\\n)", " ", txt[grep("synopsis$", rownames(txt)), lang]), 60, simplify = F),
    paste, collapse = "<br>"), '</td></tr>',
    '</div></table>')

  a <- Highcharts$new()
  a$chart(zoomType = "xy", type = 'bubble', height = height, spacing = 3)

  h2 <- hSeries2(data.frame(x = dd$x, y = dd$y, z = dd$z, name = dd$name,
    series = dd$series, url = dd$url, bname = dd$title), "series")
  a$series(h2)

  a$plotOptions(bubble = list(dataLabels = list(enabled = T,
    verticalAlign = "middle", allowOverlap = FALSE, padding = 20,
    style = list(textShadow = 'none', fontSize = "10px"),
    color = 'black', useHTML = T, formatter = "#! function() { return this.point.options.bname; } !#"),
    minSize = 20, maxSize = 60,
    cursor = "pointer",
    point = list(
     events = list(
      click = "#! function() { window.open(this.options.url); } !#")
    )))

  a$colors(c("#336666", "#366096", "#ab3d3f", "#663333", "#ac673e"))

  a$legend(borderWidth= 0, itemMarginTop = 3, itemMarginBottom = 5,
    title = list(style = list(fontWeight ='light'),
     text = paste0(txt['genre', lang],
      ' <span style="font-size: 8px; color: #666; font-weight: normal">',
      txt['clickToHide', lang], '</span><br>')
     )
  )

  a$yAxis(title = list(text = "Entrées (en milliers)"), gridLineColor = "#EFEFEF",
          labels = list(formatter = "#! function () {return this.value / 1000;} !#"))

  a$tooltip(formatter = "#! function() { return this.point.name; } !#", useHTML = T,
            borderWidth = 1, style = list(padding = 2))
  a

  hChart.html <- tempfile("hChart_cinema")
  a$save(hChart.html)

  # Convert highcharts-rCharts html chart into a responsive one
  hChart2responsiveHTML(
    hChart.html,
    output.html = paste0("top", topMovies, "_SwissMovies_bubble_", lang, ".html"),
    h2 = txt['graphic.title', lang],
    descr = paste0(txt['graphic.subtitle', lang], "<br><br>"),
    h3 = "",
    source = paste0(
      txt['source', lang], ": ",
      htmlLink(txt['source.link', lang], txt['source.label', lang]), " & ",
      htmlLink(txt['source2.link', lang], txt['source2.label', lang])),
    author = " swissinfo.ch"
    )
#}




library(isotope)

ddd <- dd
colnames(ddd)[which(colnames(ddd) == "x")] <- txt['year.cat', lang]
colnames(ddd)[which(colnames(ddd) == "y")] <- txt['admissions.cat', lang]
colnames(ddd)[which(colnames(ddd) == "z")] <- txt['imdb.cat', lang]
colnames(ddd)[which(colnames(ddd) == "note IMDb")] <- "IMDB"
colnames(ddd)[which(colnames(ddd) == "Réalisateur / Réalisatrice")] <- "director"
ddd$synopsis <- txt[grep("synopsis$", rownames(txt)), lang]
ddd$rank <- 1:nrow(ddd)


sortCols <- c(txt['year.cat', lang], txt['admissions.cat', lang], "IMDB")

tpl <- paste0(
'<div style="border: 1px solid lightgrey; margin:4px; padding:4px">
<div class="container" style="width:300px;">
<h3 class="title">{{rank}}. {{title}}</h3>
<h4>{{Titre}}</h3>
<div style="height: 180px; margin:auto">
<img src={{Affiche_pic_url}} class="circle" height="160px"/>
</div>
<p>',
  txt["year.cat", lang], ': ', '{{', txt['year.cat', lang], '}}, ',
  txt["language.cat", lang], ': {{Langue}}, ',
  txt['genre.cat', lang], ': {{series}}</p><p>',
  txt['imdb.cat', lang], ": {{IMDB}}, ",
  txt["admissions.cat", lang], ": {{entrées}}</p><p>",
  txt["director.cat", lang], ": {{director}}</p><p>",
  "{{synopsis}}",
'</p></div>
</div>'
)
isot <- isotope(ddd, layoutMode = "fitRows", filterCols = "title", sortCols = NULL, lang = 'en', elemTpl = tpl)
htmlwidgets::saveWidget(isot, "isotope_test.html", selfcontained = F, libdir = "js")


## hack !! 
regex <- '"filterBtns":"<h3>Filter\\u003c/h3><div id=\\"select-car\\">\\u003c/div>",'
css.file <- list.files(system.file("extdata", package="swiRcharts"), 'isotope_swi.css', full.names = T)
css <- read_file(css.file)

x <- readLines("isotope_test.html")
stopifnot(any(grepl(regex, x, fixed = T)))
y <- gsub(regex, "", x, fixed = TRUE)
z <- gsub("</head>", paste0(css, "</head>"), y)

cat(z, file = "isotope_test.html", sep="\n")


  

