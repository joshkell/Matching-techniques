
theme_set(theme_bw())

# MATCHING
# variables to match on:
# first-generation, age, Pell-grant eligibility, Hispanic,
# race, gender

PACE_matching <- PACE_data_rf %>%
    mutate(gender.num = as.numeric(gender),
           race = fct_infreq(race), #order AmInd/AlkNat, Asian, Black/AfAm, More than 1, Hawaiian/PI, Prefer not to say, white
           race.num = as.numeric(race),
           hispanic = hispanic.ind,
           hispanic.num = ifelse(hispanic == "N",0,1),
           first.gen.num = as.numeric(first.gen),
           pell.num = ifelse(pell == "N",0,1),
           first.term.num = as.numeric(first.term),
           ever.con = ever.concurrent,
           ever.con.num = ifelse(ever.con == "N",0,1),
           degree.seeking.num = as.numeric(degree.seeking),
           grad.num = ifelse(grad == "N",0,1),
           retention.num = ifelse(retention == "N",0,1),
           pace.num = ifelse(pace == "No",0,1)) %>%
    dplyr::select(banner.id,
                  gender, gender.num,
                  race, race.num,
                  hispanic, hispanic.num,
                  age, ever.con, ever.con.num,
                  total.prior.credits, hs.gpa.imp.ind,
                  hs.gpa, prior.credits,
                  first.term, first.term.num,
                  degree.seeking, degree.seeking.num,
                  first.gen, first.gen.num,
                  pell, pell.num,
                  retention, retention.num,
                  grad, grad.num,
                  pace, pace.num)

View(PACE_matching)

# create a dataframe of the variables we want to match on; this
# will make some things easier later on.  I'm binding one list per row, which
# isn't the most efficient way to specify the dataframe but makes it easier to
# add or remove specific rows.

matching.variables.df = bind_rows(
    list(variable = "gender.num", discrete = T, exact = T, caliper = 0),
    list(variable = "race.num", discrete = T, exact = T, caliper = 0),
    list(variable = "hispanic.num", discrete = T, exact = T, caliper = 0),
    list(variable = "first.gen.num", discrete = T, exact = T, caliper = 0),
    list(variable = "degree.seeking.num", discrete = T, exact = T, caliper = 0),
    list(variable = "hs.gpa.imp.ind", discrete = T, exact = T, caliper = 0),
    list(variable = "pell.num", discrete = T, exact = T, caliper = 0),
    list(variable = "ever.con.num", exact = T, discrete = T, caliper =0),
    list(variable = "age", exact = F, discrete = F, caliper = 0.5),
    list(variable = "first.term.num", exact = F, discrete = F, caliper = 0.5),
    list(variable = "hs.gpa", exact = F, discrete = F, caliper =0.75),
    list(variable = "prior.credits", exact = F, discrete = F, caliper =0.75)
    )

# Do matching.  We're going to match exactly on the following fields:
#   - Gender
#   - Race
#   - Hispanic
#   - First-gen status
#   - Pell eligibility
#   - HS GPA imputed ind
#   - Degree seeking
#   - ever concurrent
# And we're going to match non-exactly on the following fields:
#   - Age
#   - First semester
#   - HS GPA
#   - prior credits
# We have to feed strictly numeric values into the GenMatch() function - not
# strings or even factors.

matches = GenMatch(# The first argument to GenMatch() is a vector that indicates
    # which observations are in the treatment group.
    PACE_matching$pace.num,
    # The second argument to GenMatch() is a matrix that contains
    # ALL and ONLY the variables we want to match on.  (A
    # dataframe is okay.)
    PACE_matching[,matching.variables.df$variable],
    # The `exact` argument is a vector the same length as the
    # number of features; it specifies which features should be
    # matched exactly.
    exact = matching.variables.df$exact,
    # The `caliper` argument puts a limit on how different non-
    # exact matches can be for each feature, in standard
    # deviations.  It has to be the same length as the number of
    # features, although its values don't matter for features
    # with non-exact matching.
    caliper = matching.variables.df$caliper,
    # If we specify `replace = T`, then the same control
    # observation can be used to match more than one treatment
    # observation.
    replace = F,
    # We specify `ties = F` to tell GenMatch() to match each
    # treatment observation to no more than one control
    # observation.  Otherwise, we have to weight observations
    # based on how many matches were found.
    ties = F)

# Flag the records that were matched.  The output of GenMatch() (which I've
# saved as a variable called `matches`) itself contains several objects; the
# relevant one here is the matrix called `matches`.  Each row in `matches`
# represents a matched pair: the first column has the row index of a treatment
# observation, and the second column has the row index of the matched control
# observation.
#cache("matches")
PACE_matching$matched = F
PACE_matching$matched[unique(c(matches$matches[,1:2]))] = T

table(PACE_matching$pace, PACE_matching$matched)
# 10 pace students not matched




# this code pulls out the matches and tells me exactly which controls
# matched to which treatment observation.
# I need to do this for the SE estimate
match.pairs.df <- as.data.frame(matches$matches[,1]) %>%
    mutate(match.id = row_number()) %>%
    rename("row.id" = "matches$matches[, 1]") %>%
    bind_rows(as.data.frame(matches$matches[,2]) %>%
                  mutate(match.id = row_number()) %>%
                  rename("row.id" = "matches$matches[, 2]")
    )

matches.df <- PACE_matching %>%
    mutate(row.id = row_number()) %>%
    left_join(match.pairs.df, by = "row.id")



# How many students in the treatment group are we losing?
matches.df %>%
    filter(pace.num ==1) %>%
    mutate(across(.cols = gsub("^cs\\.", "",
                               matching.variables.df$variable[!matching.variables.df$discrete]),
                  .fns = ~ cut_interval(., 6))) %>%
    dplyr::select(banner.id, matched,
                  gsub("(\\.num$)|(^cs\\.)", "", matching.variables.df$variable)) %>%
    mutate(across(.cols = -c(banner.id, matched), .fns = ~ as.character(.))) %>%
    pivot_longer(cols = -c(banner.id, matched), names_to = "variable",
                 values_to = "value") %>%
    ggplot(aes(x = value, fill = matched)) +
    geom_bar() +
    scale_x_discrete("", labels = wrap_format(15)) +
    scale_y_continuous("Number of students in the treatment group") +
    scale_fill_manual("Matched?", values = dsa.colors(c(7, 1))) +
    facet_wrap(~ variable, scales = "free") +
    coord_flip()
# we have matched all PACE students

# Check matching on continuous predictors.
bind_rows(
    matches.df %>%
        mutate(matching = "Before matching"),
    matches.df %>%
        filter(matched) %>%
        mutate(matching = "After matching")
) %>%
    mutate(matching = fct_relevel(matching, "Before matching",
                                  "After matching")) %>%
    dplyr::select(matching, pace,
                  gsub("^cs\\.", "",
                       c(matching.variables.df$variable[!matching.variables.df$discrete]
                         ))) %>%
    pivot_longer(cols = -c(matching, pace), names_to = "variable",
                 values_to = "value") %>%
    group_by(matching, variable, pace) %>%
    mutate(percentile = ntile(value, 100)) %>%
    ungroup() %>%
    group_by(matching, variable, pace, percentile) %>%
    summarise(mean.value = mean(value), .groups = "keep") %>%
    ungroup() %>%
    pivot_wider(id_cols = c(matching, variable, percentile),
                names_from = "pace", values_from = "mean.value") %>%
    ggplot(aes(x = `No`, y = `Yes`)) +
    geom_point(color = dsa.colors(1), position = "jitter", alpha = 0.2) +
    geom_abline(slope = 1, intercept = 0) +
    facet_wrap(~ variable + matching, ncol = 2, scales = "free") +
    scale_x_continuous("Students not served by pace ") +
    scale_y_continuous("Students served by pace")

# Check matching on discrete predictors.
bind_rows(
    matches.df %>%
        mutate(matching = "Before matching"),
    matches.df %>%
        filter(matched) %>%
        mutate(matching = "After matching")
) %>%
    mutate(matching = fct_relevel(matching, "Before matching",
                                  "After matching")) %>%
    dplyr::select(matching, pace,
                  gsub("\\.num$", "", matching.variables.df$variable[matching.variables.df$discrete])) %>%
    mutate(across(.cols = -c(matching, pace), .fns = ~ as.character(.))) %>%
    pivot_longer(cols = -c(matching, pace), names_to = "variable",
                 values_to = "value") %>%
    group_by(matching, pace, variable, value) %>%
    summarise(n.students = n(), .groups = "keep") %>%
    ungroup() %>%
    group_by(matching, pace, variable) %>%
    mutate(prop = n.students / sum(n.students)) %>%
    ungroup() %>%
    ggplot(aes(x = pace, y = prop, fill = value)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(matching ~ variable, scales = "free") +
    scale_x_discrete("Served by pace?") +
    scale_y_continuous("Percent of students", labels = percent_format())




doublecheck <- subset(matches.df,
                      is.na(matches.df$term.num))

#cache("matches.df")
