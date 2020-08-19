import pandas as pd
import numpy as np
import copy


def generate(file_name, year):
    '''
    This function takes a factor data file and year and determines which companies (identified by permno)
    to long and which to short by computing a combination of their factors
    '''


    # Read factor data in and get data for specified year
    data = pd.read_csv(file_name)
    last_year = data.query("year==" + str(year-1))
    data = data.query("year==" + str(year))
    
    

    # Profitability factor from GProf (Gross Profit/Total Assets) and sale_invcap (Sales/Invested Capital)
    # Coefficients based on results from fama-macbeth regression
    profitability_col = data.loc[:,"GProf"] + .75 * data.loc[:,"sale_invcap"] + \
         .16 * data.loc[:,"roa"]
    data.loc[:,"profitability"] = profitability_col
    
    last_prof = last_year.loc[:,"GProf"] + .75 * last_year.loc[:,"sale_invcap"] + \
         .16 * last_year.loc[:,"roa"]
    data["fund_mom"] = profitability_col / last_prof
     # fund_mom measures "momentum" of the profitability factors year to year
    
    weight_dict = {
        # Profitability-related factors
        'profitability': 3.3,
        'fund_mom': 3.3,

        # Value-related factors
        'b_hml': 1.3, # Beta coefficient of High Minus Low factor
        'PEG_ltgforward': -2.6, # Forward P/E to Long Term Growth

        }

    # List of factors being used
    all_factors = list(weight_dict.keys())

  # Z-Scoring all the factors and storing them into factor_data
    factor_data = pd.DataFrame()
    for factor_name in all_factors:
        # Z-Scores each factor in data by industry
        factor_data[factor_name] = data.groupby(
            ['INDUSTRY'])[factor_name].transform(lambda x: (x - x.mean()) / x.std())

    # Winsorizing and filling NaNs
    factor_data.clip(-3, 3, inplace=True)
    factor_data.fillna(0, inplace=True)
    factor_data['permno'] = data[['permno']]

    # initializing all the total_scores to 0
    factor_data['total_score'] = [0] * factor_data.shape[0]

    # For each factor
    for each_factor in all_factors:
        # Adding the weight * factor to the companies total_score
        factor_data['total_score'] = weight_dict[each_factor] * \
            factor_data[each_factor] + factor_data['total_score']

    # Sort companies by their total_score
    factor_data.sort_values(by='total_score', ascending=False, inplace=True)

    # Total number of companies
    total_number = factor_data.shape[0]

    # Number of stocks to long and short (Here it is the top and bottom 10%)
    long_short_count = int(0.1 * total_number)

    # Take the top N stocks to long
    long = factor_data.head(long_short_count)['permno'].to_numpy()
    short = factor_data.tail(long_short_count)['permno'].to_numpy()

    # Create dataframe with two columns one to designate long-short and permno to identify the company
    target = []
    for each_item in long:
        target.append(['L', each_item])
    for each_item in short:
        target.append(['S', each_item])

    pd.DataFrame(target, columns= ['LS', 'permno']).to_csv('./output/%s.csv' % (year), index = False)

if __name__ == "__main__":
    generate("outofsample1.csv", 2009)
