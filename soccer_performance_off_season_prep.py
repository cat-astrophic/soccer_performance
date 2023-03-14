# This script identifies the sample for a paper using an RDD to look at performance

# Importing required modules

import pandas as pd
from ast import literal_eval

# Project directory

direc = 'F:/soccer_performance/'

# Read in the data

qualifiers = pd.read_csv(direc + 'data/qualifiers.csv')
data = pd.read_csv(direc + 'data/raw_data.csv')

# Defining a function for finding cutoffs

def cutofffinder(small_df):
    
    cont = True
    idx = 0
    
    while cont == True:
        
        if small_df['European Play'][idx+1] == 0:
            
            cont = False
            
        else:
            
            idx += 1
    
    return idx

# Initializing data storage

cl0 = []
cl1 = []
el0 = []
el1 = []

# Creating the various treatment variables

leagues = list(data.League.unique())
seasons = list(data.Season.unique())

for l in leagues:
    
    tmpq = qualifiers[qualifiers.League == l]
    tmpl = data[data.League == l]
    
    for s in seasons:
        
        print('Creating treatment variables for ' + str(s) + ' in the ' + l)
        
        tmpqs = tmpq[tmpq.Year == s].reset_index(drop = True)
        tmpls = tmpl[tmpl.Season == s].reset_index(drop = True)
        
        cl_gs = literal_eval(tmpqs.CL_GS[0])
        cl_q = literal_eval(tmpqs.CL_Q[0])
        el_gs = literal_eval(tmpqs.EL_GS[0])
        el_q = literal_eval(tmpqs.EL_Q[0])
        
        for i in range(1,1+len(tmpls)):
            
            cl0.append(int(i in cl_gs))
            cl1.append(int(i in cl_q))
            el0.append(int(i in el_gs))
            el1.append(int(i in el_q))

cl = [cl0[x] + cl1[x] for x in range(len(cl0))]
el = [el0[x] + el1[x] for x in range(len(el0))]
euro = [cl[x] + el[x] for x in range(len(cl))]

# Expanding the dataframe

cl0 = pd.Series(cl0, name = 'Champions League - Group State')
cl1 = pd.Series(cl1, name = 'Champions League - Qualifier')
el0 = pd.Series(el0, name = 'Europa League - Group Stage')
el1 = pd.Series(el1, name = 'Europa League - Qualifier')
cl = pd.Series(cl, name = 'Champions League')
el = pd.Series(el, name = 'Europa League')
euro = pd.Series(euro, name = 'European Play')

data = pd.concat([data, cl0, cl1, el0, el1, cl, el, euro], axis = 1)
ref_data = data

# Loop through time windows and calculate the outcome variables

for z in range(1,9):
    
    seasons = seasons[:-1]
    data = data[data.Season < max(data.Season)].reset_index(drop = True)
    
    change_in_place = []
    change_in_points = []
    change_in_wins = []
    change_in_draws = []
    change_in_losses = []
    change_in_scored = []
    change_in_allowed = []
    change_in_diff = []
    change_in_cl = []
    change_in_el = []
    change_in_euro = []
    
    level_in_place = []
    level_in_points = []
    level_in_wins = []
    level_in_draws = []
    level_in_losses = []
    level_in_scored = []
    level_in_allowed = []
    level_in_diff = []
    level_in_cl = []
    level_in_el = []
    level_in_euro = []
    
    lag_in_place = []
    lag_in_points = []
    lag_in_wins = []
    lag_in_draws = []
    lag_in_losses = []
    lag_in_scored = []
    lag_in_allowed = []
    lag_in_diff = []
    lag_in_cl = []
    lag_in_el = []
    lag_in_euro = []
    
    top_flight = []
    
    for i in range(len(data)):
        
        print('Time window ' + str(z) + ' of 8 :: Calculating outcomes for observation ' + str(i+1) + ' of ' + str(len(data)) + '.......')
        
        tmp = ref_data[ref_data.League == data.League[i]]
        tmp = tmp[tmp.Season == data.Season[i] + z]
        tmp = tmp[tmp.Team == data.Team[i]].reset_index(drop = True)
        
        try:
            
            change_in_place.append(tmp.Place[0] - data.Place[i])
            change_in_points.append(tmp.Points[0] - data.Points[i])
            change_in_wins.append(tmp.Wins[0] - data.Wins[i])
            change_in_draws.append(tmp.Draws[0] - data.Draws[i])
            change_in_losses.append(tmp.Losses[0] - data.Losses[i])
            change_in_scored.append(tmp.Goals_For[0] - data.Goals_For[i])
            change_in_allowed.append(tmp.Goals_Allowed[0] - data.Goals_Allowed[i])
            change_in_diff.append(tmp.Goal_Difference[0] - data.Goal_Difference[i])
            change_in_cl.append(tmp['Champions League'][0] - data['Champions League'][i])
            change_in_el.append(tmp['Europa League'][0] - data['Europa League'][i])
            change_in_euro.append(tmp['European Play'][0] - data['European Play'][i])
            
            level_in_place.append(tmp.Place[0])
            level_in_points.append(tmp.Points[0])
            level_in_wins.append(tmp.Wins[0])
            level_in_draws.append(tmp.Draws[0])
            level_in_losses.append(tmp.Losses[0])
            level_in_scored.append(tmp.Goals_For[0])
            level_in_allowed.append(tmp.Goals_Allowed[0])
            level_in_diff.append(tmp.Goal_Difference[0])
            level_in_cl.append(tmp['Champions League'][0])
            level_in_el.append(tmp['Europa League'][0])
            level_in_euro.append(tmp['European Play'][0])
            
            lag_in_place.append(data.Place[i])
            lag_in_points.append(data.Points[i])
            lag_in_wins.append(data.Wins[i])
            lag_in_draws.append(data.Draws[i])
            lag_in_losses.append(data.Losses[i])
            lag_in_scored.append(data.Goals_For[i])
            lag_in_allowed.append(data.Goals_Allowed[i])
            lag_in_diff.append(data.Goal_Difference[i])
            lag_in_cl.append(data['Champions League'][i])
            lag_in_el.append(data['Europa League'][i])
            lag_in_euro.append(data['European Play'][i])
            
            top_flight.append(1)
            
        except:
            
            change_in_place.append(None)
            change_in_points.append(None)
            change_in_wins.append(None)
            change_in_draws.append(None)
            change_in_losses.append(None)
            change_in_scored.append(None)
            change_in_allowed.append(None)
            change_in_diff.append(None)
            change_in_cl.append(None)
            change_in_el.append(None)
            change_in_euro.append(None)
            
            level_in_place.append(None)
            level_in_points.append(None)
            level_in_wins.append(None)
            level_in_draws.append(None)
            level_in_losses.append(None)
            level_in_scored.append(None)
            level_in_allowed.append(None)
            level_in_diff.append(None)
            level_in_cl.append(None)
            level_in_el.append(None)
            level_in_euro.append(None)
            
            lag_in_place.append(data.Place[i])
            lag_in_points.append(data.Points[i])
            lag_in_wins.append(data.Wins[i])
            lag_in_draws.append(data.Draws[i])
            lag_in_losses.append(data.Losses[i])
            lag_in_scored.append(data.Goals_For[i])
            lag_in_allowed.append(data.Goals_Allowed[i])
            lag_in_diff.append(data.Goal_Difference[i])
            lag_in_cl.append(data['Champions League'][i])
            lag_in_el.append(data['Europa League'][i])
            lag_in_euro.append(data['European Play'][i])
            
            top_flight.append(0)
            
    # Add outcome variables to the dataframe
    
    change_in_place = pd.Series(change_in_place, name = 'Change in Place')
    change_in_points = pd.Series(change_in_points, name = 'Change in Points')
    change_in_wins = pd.Series(change_in_wins, name = 'Change in Wins')
    change_in_draws = pd.Series(change_in_draws, name = 'Change in Draws')
    change_in_losses = pd.Series(change_in_losses, name = 'Change in Losses')
    change_in_scored = pd.Series(change_in_scored, name = 'Change in Goals Scored')
    change_in_allowed = pd.Series(change_in_allowed, name = 'Change in Goals Allowed')
    change_in_diff = pd.Series(change_in_diff, name = 'Change in Goal Differential')
    change_in_cl = pd.Series(change_in_cl, name = 'Change in Champions League')
    change_in_el = pd.Series(change_in_el, name = 'Change in Europa League')
    change_in_euro = pd.Series(change_in_euro, name = 'Change in European Play')
    
    level_in_place = pd.Series(level_in_place, name = 'Place - Final')
    level_in_points = pd.Series(level_in_points, name = 'Points - Final')
    level_in_wins = pd.Series(level_in_wins, name = 'Wins - Final')
    level_in_draws = pd.Series(level_in_draws, name = 'Draws - Final')
    level_in_losses = pd.Series(level_in_losses, name = 'Losses - Final')
    level_in_scored = pd.Series(level_in_scored, name = 'Goals Scored - Final')
    level_in_allowed = pd.Series(level_in_allowed, name = 'Goals Allowed - Final')
    level_in_diff = pd.Series(level_in_diff, name = 'Goal Differential - Final')
    level_in_cl = pd.Series(level_in_cl, name = 'Champions League - Final')
    level_in_el = pd.Series(level_in_el, name = 'Europa League - Final')
    level_in_euro = pd.Series(level_in_euro, name = 'European Play - Final')
    
    lag_in_place = pd.Series(lag_in_place, name = 'Place - Initial')
    lag_in_points = pd.Series(lag_in_points, name = 'Points - Initial')
    lag_in_wins = pd.Series(lag_in_wins, name = 'Wins - Initial')
    lag_in_draws = pd.Series(lag_in_draws, name = 'Draws - Initial')
    lag_in_losses = pd.Series(lag_in_losses, name = 'Losses - Initial')
    lag_in_scored = pd.Series(lag_in_scored, name = 'Goals Scored - Initial')
    lag_in_allowed = pd.Series(lag_in_allowed, name = 'Goals Allowed - Initial')
    lag_in_diff = pd.Series(lag_in_diff, name = 'Goal Differential - Initial')
    lag_in_cl = pd.Series(lag_in_cl, name = 'Champions League - Initial')
    lag_in_el = pd.Series(lag_in_el, name = 'Europa League - Initial')
    lag_in_euro = pd.Series(lag_in_euro, name = 'European Play - Initial')
    
    top_flight = pd.Series(top_flight, name = 'Top Flight')
    
    data_x = pd.concat([data, change_in_place, change_in_points, change_in_wins, change_in_draws,
                        change_in_losses, change_in_scored, change_in_allowed, change_in_diff,
                        change_in_cl, change_in_el, change_in_euro, level_in_place, level_in_points,
                        level_in_wins, level_in_draws, level_in_losses, level_in_scored,
                        level_in_allowed, level_in_diff, level_in_cl, level_in_el, level_in_euro,
                        lag_in_place, lag_in_points, lag_in_wins, lag_in_draws, lag_in_losses,
                        lag_in_scored, lag_in_allowed, lag_in_diff, lag_in_cl, lag_in_el,
                        lag_in_euro, top_flight], axis = 1)
    
    # Define the sample
    
    windows = [x for x in range(1,9)]
    window_list = []
    league_list = []
    season_list = []
    cutoff_list = []
    treated_lists = []
    control_lists = []
    treated_teams = []
    control_teams = []
    
    for w in windows:
        
        for l in leagues:
            
            tmp = data_x[data_x.League == l]
            
            for s in seasons:
                
                print('Time window ' + str(z) + ' of 8 :: Season - ' + str(s) + ' :: League - ' + l + ' :: Window - ' + str(w) + '.......')
                
                tmps = tmp[tmp.Season == s].reset_index(drop = True)
                cutoff = cutofffinder(tmps) # last team to qualify from place in table
                cutoff_list.append(cutoff)
                
                tmp_in = tmps[tmps.Points >= tmps.Points[cutoff]]
                tmp_out = tmps[tmps.Points < tmps.Points[cutoff]]
                tmp_in = tmp_in[tmp_in.Points <= tmps.Points[cutoff] + w]
                tmp_out = tmp_out[tmp_out.Points >= tmps.Points[cutoff] - w]
                
                treated_lists.append(list(tmp_in.Place))
                control_lists.append(list(tmp_out.Place))
                window_list.append(w)
                league_list.append(l)
                season_list.append(s)
                treated_teams.append(list(tmp_in.Team))
                control_teams.append(list(tmp_out.Team))
    
    # Make a dataframe for the final samples
    
    windows_ = []
    leagues_ = []
    seasons_ = []
    cutoffs_ = []
    treated_ = []
    control_ = []
    places_ = []
    teams_ = []
    
    for t in range(len(treated_lists)):
        
        for p in range(len(treated_lists[t])):
            
            windows_.append(window_list[t])
            leagues_.append(league_list[t])
            seasons_.append(season_list[t])
            cutoffs_.append(cutoff_list[t])
            treated_.append(1)
            control_.append(0)
            places_.append(treated_lists[t][p])
            teams_.append(treated_teams[t][p])
            
    for t in range(len(control_lists)):
        
        for p in range(len(control_lists[t])):
            
            windows_.append(window_list[t])
            leagues_.append(league_list[t])
            seasons_.append(season_list[t])
            cutoffs_.append(cutoff_list[t])
            treated_.append(0)
            control_.append(1)
            places_.append(control_lists[t][p])
            teams_.append(control_teams[t][p])
    
    windows_ = pd.Series(windows_, name = 'Window')
    leagues_ = pd.Series(leagues_, name = 'League')
    seasons_ = pd.Series(seasons_, name = 'Season')
    cutoffs_ = pd.Series(cutoffs_, name = 'Cutoff')
    treated_ = pd.Series(treated_, name = 'Treated')
    control_ = pd.Series(control_, name = 'Control')
    places_ = pd.Series(places_, name = 'Place')
    teams_ = pd.Series(teams_, name = 'Team')
    
    sample_df = pd.concat([windows_, leagues_, seasons_, cutoffs_, treated_, control_, places_, teams_], axis = 1)
    
    # Make sure the control group does not include any of the weird cases where a non-standard entrant to European play occurred
    
    drops = []
    
    for i in range(len(sample_df)):
        
        if sample_df.Treated[i] == 0:
            
            tmp = data_x[data_x.League == sample_df.League[i]]
            tmp = tmp[tmp.Team == sample_df.Team[i]]
            tmp = tmp[tmp.Season == sample_df.Season[i]].reset_index(drop = True)
            
            if tmp['European Play'][0] == 1:
                
                drops.append(i)
    
    sample_df = sample_df[~sample_df.index.isin(drops)].reset_index(drop = True)
    
    # Merge data frames
    
    merged_df = pd.merge(sample_df, data_x, how = 'inner', on = ['League', 'Season', 'Team'])
    merged_df = merged_df.rename(columns = {'Place_x':'Place'})
    merged_df = merged_df.drop(['Place_y'], axis = 1)
    
    # Save to file
    
    merged_df.to_csv(direc + 'data/data_' + str(z) + '_.csv', index = False)

