## String maniputlation
## David Wilkinson
## Uni Melb, 2020

#####################
### Load Packages ###
#####################

library(dplyr)

######################
### "Load" Dataset ###
######################

starwars

starwars_samp <- head(starwars)

####################
### Find Matches ###
####################

grep(pattern = "Skywalker",
     x = starwars_samp$name,
     ignore.case = FALSE,
     value = FALSE,
     invert = FALSE)

grepl(pattern = "Skywalker",
      x = starwars_samp$name,
      ignore.case = FALSE)

stringi::stri_detect_fixed(starwars_samp$name,
                           pattern = c("Skywalker", "C", "Darth"))

stringr::str_detect(starwars_samp$name,
                    pattern = c("Skywalker", "C", "Darth"),
                    negate = TRUE)

stringi::stri_locate(starwars_samp$name,
                     regex = "\\d",
                     mode = "first")

stringi::stri_locate(starwars_samp$name,
                     regex = "\\d",
                     mode = "all")

stringr::str_locate(starwars_samp$name,
                    pattern = "\\d")

stringr::str_locate_all(starwars_samp$name,
                        pattern = "\\d")

stringi::stri_duplicated(c(starwars_samp$name, 
                           head(starwars$name)))

###########################
### Pattern Replacement ###
###########################

starwars_samp$name

gsub("Skywalker",
     "SKYWALKER",
     starwars_samp$name)

gsub(" ",
     "_",
     starwars_samp$name)

sub(" ",
    "_",
    starwars$name)

stringr::str_replace(starwars_samp$name,
                     pattern = c("Luke", "Leia", "-"),
                     replacement = c("Leia", "AAA", "Luke"))

##########################
### Pattern Extraction ###
##########################

stringi::stri_extract(starwars_samp$name,
                      regex = "\\d",
                      mode = "first")

stringi::stri_extract(starwars_samp$name,
                      regex = "\\d",
                      mode = "last")

stringi::stri_extract(starwars_samp$name,
                      regex = "\\d",
                      mode = "all")

stringr::str_extract(starwars_samp$name,
                     pattern = "\\d")

stringr::str_extract_all(starwars_samp$name,
                         pattern = "\\d",
                         simplify = TRUE)

stringi::stri_count_regex(starwars_samp$name,
                          pattern = "\\d")

########################
### String Splitting ###
########################

strsplit(starwars_samp$name,
         split = " ")

stringr::str_split(starwars_samp$name,
                   pattern = " ")

stringr::str_split(starwars_samp$name,
                   pattern = " ",
                   simplify = TRUE)

stringr::str_split_fixed(starwars_samp$name,
                         pattern = " ",
                         n = 1)

stringr::str_split_fixed(starwars$name,
                         pattern = " ",
                         n = 2)

stringr::str_pad(starwars_samp$name,
                 width = 30)

stringr::str_pad(starwars_samp$name,
                 width = 30,
                 side = "right")

stringr::str_pad(starwars_samp$name,
                 width = 30,
                 side = "both")

stringr::str_pad(starwars_samp$name,
                 width = 30,
                 side = "both",
                 pad = "A")

########################
### Building Strings ###
########################

## See the sprintf.html file I sent alongside this R script
## for a better sprintf tutorial

person <- charlatan::ch_name(1)

job <- charlatan::ch_job(1)

phone_number <- charlatan::ch_phone_number(1)

paste(person, "worked as a", job, ". Their work phone number is", phone_number)

sprintf("%s worked as a %s. Their work phone number is %s.",
        person,
        job,
        phone_number)

sprintf("%s worked as a %s. Their work phone number is %s.",
        charlatan::ch_name(1, locale = "ko_KR"),
        charlatan::ch_job(1, locale = "ru_RU"),
        charlatan::ch_phone_number(1, locale = "sv_SE"))

sprintf("%1$s     %1$s     %1$s     %1$s",
        person)

sprintf("%d",
        1)

filename <- sprintf("Outputs/%s/%s_plot.pdf",
                    person,
                    job)

pdf(filename)

command <- sprintf("aaa <- sum(%d,%d)",
                   1,
                   1:2)

eval(parse(text = command))

command <- sprintf("a%s <- sum(%s,%s)",
                   1:2,
                   1:2)

filename

basename(filename)
dirname(filename)
