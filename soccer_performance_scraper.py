# This script scrapes soccer data from ESPN

# Importing required modules

import urllib
from bs4 import BeautifulSoup as bs
import pandas as pd

# Project directory

direc = 'F:/soccer_performance/'

# Defining the url components

base = 'https://www.espn.com/soccer/standings/_/league/'
eng_ = 'ENG.1/season/'
esp_ = 'esp.1/season/'
fra_ = 'fra.1/season/'
ita_ = 'ita.1/season/'
deu_ = 'ger.1/season/'

leagues_ = [eng_, esp_, fra_, ita_, deu_]
league_names = ['EPL', 'La Liga', 'Serie A', 'Ligue Un', 'Bundesliga']
years_ = [x for x in range(2003, 2022)]

# Initiializing data storage

league = []
year = []
place = []
team = []
points = []
games = []
wins = []
draws = []
losses = []
goals_for = []
goals_allowed = []
goal_diff = []
data_list = [games, wins, losses, draws, goals_for, goals_allowed, goal_diff, points]

# Main loop

for l in leagues_:
    
    for y in years_:
        
        print('Scraping data for season :: ' + str(y+1) + ' :: from league ' + l)
        
        url = base + l + str(y)
        page = urllib.request.Request(url, headers = {'User-Agent': 'Mozilla/5.0'})
        response = urllib.request.urlopen(page)
        soup = bs(response, 'html.parser')
        data = soup.find_all('tbody')
        team_table = data[0].find_all('td')
        data_table = data[1].find_all('td')
        
        for t in team_table:
            
            idx_team = str(t.find_all('span')[2]).index('title="')
            xxx = str(t.find_all('span')[2])[idx_team + 7:]
            idx_team2 = xxx.index('">')
            team.append(xxx[:idx_team2])
            league.append(league_names[leagues_.index(l)])
            year.append(y+1)
            place.append(int(team_table.index(t)+1))
            
        for i in range(len(team_table)):
            
            for j in range(8):
                
                xxx = str(data_table[i*8 + j])
                
                try:
                    
                    idx_data = xxx.index('"stat-cell">')
                    idx_data2 = xxx[idx_data + 12:].index('</span>')
                    text = xxx[idx_data + 12:idx_data + 12 + idx_data2]
                    data_list[j].append(text)
                    
                except:
                    
                    idx_data = xxx.index('ive">')
                    idx_data2 = xxx[idx_data + 5:].index('</span>')
                    text = xxx[idx_data + 5:idx_data + 5 + idx_data2]
                    data_list[j].append(str(text.replace('+','')))
                    
# Making a dataframe

league = pd.Series(league, name = 'League')
year = pd.Series(year, name = 'Season')
place = pd.Series(place, name = 'Place')
team = pd.Series(team, name = 'Team')
points = pd.Series(points, name = 'Points')
games = pd.Series(games, name = 'Games_Played')
wins = pd.Series(wins, name = 'Wins')
draws = pd.Series(draws, name = 'Draws')
losses = pd.Series(losses, name = 'Losses')
goals_for = pd.Series(goals_for, name = 'Goals_For')
goals_allowed = pd.Series(goals_allowed, name = 'Goals_Allowed')
goal_diff = pd.Series(goal_diff, name = 'Goal_Difference')

soccer_df = pd.concat([league, year, place, team, points, wins, draws, losses, goals_for, goals_allowed, goal_diff], axis = 1)

# Saving the dataframe to file

soccer_df.to_csv(direc + 'data/raw_data.csv', index = False, encoding = 'utf-8-sig')

