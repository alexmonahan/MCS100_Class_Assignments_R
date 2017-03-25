# This tutorial uses the R package rvest, which helps you easily scrape data
# from web pages directly into R.

require(rvest)

# Below I've spotted you the 12 teams in the WNBA, along with their abbreviated
# identifiers used by ESPN. You could probably do this automatically, but with
# only 12 teams, it was easy enough to do manually.

teams = c('Atlanta', 'Chicago', 'Connecticut', 'Dallas', 'Indiana',
          'Los Angeles', 'Minnesota', 'NY Liberty', 'Phoenix', 'San Antonio',
          'Seattle', 'Washington')
names(teams) = c('atl', 'chi', 'conn', 'dal', 'ind', 'la', 'min', 'ny', 'phx',
                 'sa', 'sea', 'wsh')

# The html() function parses an HTML page and stores the result in R, allowing
# you to scrape data from it. Let's get the web page containing the Chicago
# Sky's 2015 schedule and results.

page = html('http://espn.go.com/wnba/team/schedule/_/name/chi/year/2015')

# The html_nodes() function extracts all pieces of an HTML document that match
# a specified CSS selector. To practice building CSS selectors through a fun
# exercise, check out http://flukeout.github.io/. The selector 'a' will select
# all sections of the HTML document with the <a></a> tag, which means
# hyperlink. Then we can use html_text() to return the text from those tags.

text = html_text(html_nodes(page, 'a'))

# Let's use slightly more involved CSS selectors to extract the opponents,
# results and scores from the Sky's 2015 schedule. For each game, let's also
# identify whether the Sky are the home team or the away team.

opponent = html_text(html_nodes(page, 'li.team-name a'))
score = html_text(html_nodes(page, 'li.score a'))
result = html_text(html_nodes(page, 'li.game-status span'))
home = html_text(html_nodes(page, 'li.game-status'))

# We see that we're going to start running into problems when the data we need
# are not formatted in such a way that it is easy to pull them all at once from
# the web page. Let's work interactively now to build a scraper that goes
# through each row of the schedule table to scrape the information from each
# game. We can use the CSS selector 'tr' to get all table rows. Once we
# successfully scrape the data from one team's page, let's loop over all of the
# teams and scrape the data for each team's home games, thus recording each
# game only once.

i = 0
raw = matrix('', 250, 4)
colnames(raw) = c('date', 'home', 'away', 'result')
for (t in names(teams)) {
  print(teams[t])
  page = html(paste('http://espn.go.com/wnba/team/schedule/_/name/', t,
                    '/year/2015', sep = ''))
  regular.season = 0
  for (row in html_nodes(page, 'tr')) {
    if (regular.season == 1) {
      if(grepl('Preseason', html_text(row))) {
        regular.season = 2
        break()
      }
      if (html_attrs(row) != "colhead") {
        date = html_text(row[[1]])
        opponent.field = row[[2]]
        home = html_text(html_nodes(opponent.field, '.game-status'))
        opponent = html_text(html_nodes(opponent.field, '.team-name'))
        result = html_text(row[[3]])
        if (home == 'vs') {
          i = i + 1
          raw[i, ] = c(date, teams[t], opponent, result)
        }
      }
    }
    if (regular.season == 0) {
      if(grepl('Regular Season', html_text(row))) {
        regular.season = 1
      }
    }
  }
}
raw = raw[1:i, ]

# We have successfully completed the scraping of the data, and what remains is
# to clean the data in R so that we could use this dataset to build, for
# example, a Bradley-Terry model.

# First, delete "Postponed" games.

data = raw[raw[, 'result'] != 'Postponed', ]

# Second, let's parse the "result" column to get the home and away scores.

# We split the result column by " " to separate the "OT" string from the score
# itself, for games which went into overtime.

result.split = strsplit(data[, 'result'], ' ')
result.clean = sapply(result.split, function(x)x[1])
ot = sapply(result.split, function(x)x[2])
ot[is.na(ot)] = ''

# The winning team's score is always listed first, so to find the home team's
# score, we need to check whether the home team won. We can look at the first
# character in the result to find out.

win = substring(result.clean, 1, 1) == 'W'

# Next, we split the score string by "-" to separate the winning score from the
# losing score, and we assing the appropriate score to the home and away teams.

score = strsplit(substring(result.clean, 2, 8), '-')
score.winner = sapply(score, function(x)x[1])
score.loser = sapply(score, function(x)x[2])
score.home = ifelse(win, score.winner, score.loser)
score.away = ifelse(!win, score.winner, score.loser)

# Less necessary, below I parse the date of the game so that R can sort the
# game results chronologically.

months = 6:9
names(months) = c('Jun', 'Jul', 'Aug', 'Sep')
month = months[substring(data[, 'date'], 6, 8)]
day = as.numeric(substring(data[, 'date'], 10))
date = as.Date(paste(2015, month, day, sep = '-'))

# Finally, we have a clean dataset of scores from the 2015 WNBA season! Let's
# use data.frame() to store the data and write.csv() to write the data to a CSV
# file!

wnba15 = data.frame(date = date, home = data[, 'home'],
                    home.score = score.home, away = data[, 'away'], away.score = score.away,
                    overtime = ot)
wnba15 = wnba15[order(wnba15$date), ]
write.csv(wnba15, file = 'wnba15.csv', row.names = FALSE, quote = FALSE)

# That's it! For more material to help you learn how to use rvest, check out
# the rvest tutorial under the Handouts tab of the Stats 50 web site!
