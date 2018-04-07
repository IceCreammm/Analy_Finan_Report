import urllib
import json
import pandas as pd
import sqlite3
from sqlalchemy import create_engine

######################################## API function #############################################
## this function get token from API
def get_token(client_id, client_secret):
    #""""
    ## this function is to generate token from cninfo API
    #""""
    url = 'http://webapi.cninfo.com.cn/api-cloud-platform/oauth2/token'
    post_data = "grant_type=client_credentials&client_id=%s&client_secret=%s"%(client_id, client_secret)
    req = urllib.urlopen(url, post_data)
    responsedict = json.loads(req.read())
    token = responsedict["access_token"]
    return token


def get_data_post(inp_url, id_stock, val_token, rdate='', format='json'):
    post_data = "scode=%s&access_token=%s&rdate=%s&format=%s"%(id_stock, val_token, rdate, format)
    req = urllib.urlopen(inp_url, post_data)
    content = req.read()
    return content

def get_data_post_all_date(inp_url, id_stock, val_token, format='json'):
    post_data = "scode=%s&access_token=%s&format=%s"%(id_stock, val_token, format)
    req = urllib.urlopen(inp_url, post_data)
    content = req.read()
    return content
##########################################################################################


###################################### GET stock id to update ##################################
dir_data = '/Users/JasonWang/Documents/Projects_R/Analy_Finan_Report/DATA/'
dir_db = dir_data + 'CLEAN/db_finan_report.sqlite'
# Connecting to the database file
sqlite_db = sqlite3.connect(dir_db)
conn_db = sqlite_db.cursor()
# get stock id and date
conn_db.execute("SELECT DISTINCT {idf} FROM {tn} ".format(idf= "SECCODE, F001D", tn='tabl_ts_bal_sheet'))
res = conn_db.fetchall()
df_id_date = pd.DataFrame.from_records(res, columns = ['id', 'date'])
df_id_date_last = df_id_date.groupby('id').agg(max)
df_sel = df_id_date_last.loc[df_id_date_last['date'] != '2017-12-31']
# get unique stock id
conn_db.execute("SELECT DISTINCT {idf} FROM {tn} ".format(idf= "SECCODE", tn='tabl_ts_bal_sheet'))
res = conn_db.fetchall()
lst_uni_id = list(pd.DataFrame.from_records(res, columns = ['id']).id)
##################################################################################################



############################################################################################################

# dict_cninfo_api_port = {'bal': 'p_stock2300', 'pnl': 'p_stock2301', 'cash': 'p_stock2302', 'key_ind': 'p_stock2303'}
# get token
client_id = 'aa3e9ab1d4f342c3a40a7aaafeaa3af4'
client_pass = 'dfada07b3867443a9c333327448545cb'
token = get_token(client_id, client_pass)
url_api = 'http://webapi.cninfo.com.cn/api/stock/'
# prepare loop over id for collecting API data
lst_bal = []
lst_pnl = []
lst_cash = []
lst_indi = []


for idx in range(1, len(lst_uni_id)/10 + 2):
    char_uni_id = ','.join(lst_uni_id[((idx - 1) * 10):min(10*idx, len(lst_uni_id))])
    print(char_uni_id)
    ### get BAL data from API
    json_bal = json.loads(get_data_post_all_date(url_api + 'p_stock2300', char_uni_id, token, 'json'))
    lst_bal.append(pd.DataFrame(json_bal['records'])) if json_bal['resultmsg'] == 'success' and len(
         json_bal['records']) >= 1 else ''
    ### get PNL data from API
    json_bal = json.loads(get_data_post_all_date(url_api + 'p_stock2301', char_uni_id, token, 'json'))
    lst_pnl.append(pd.DataFrame(json_bal['records'])) if json_bal['resultmsg'] == 'success' and len(
        json_bal['records']) >= 1 else ''
    ### get CASH data from API
    json_bal = json.loads(get_data_post_all_date(url_api + 'p_stock2302', char_uni_id, token, 'json'))
    lst_cash.append(pd.DataFrame(json_bal['records'])) if json_bal['resultmsg'] == 'success' and len(
        json_bal['records']) >= 1 else ''
    ### get INDI data from API
    json_bal = json.loads(get_data_post_all_date(url_api + 'p_stock2303', char_uni_id, token, 'json'))
    lst_indi.append(pd.DataFrame(json_bal['records'])) if json_bal['resultmsg'] == 'success' and len(
        json_bal['records']) >= 1 else ''


### convert collection list into df
df_bal = pd.concat(lst_bal, axis=0)
df_pnl = pd.concat(lst_pnl, axis=0)
df_cash = pd.concat(lst_cash, axis=0)
df_indi = pd.concat(lst_indi, axis=0)



### write dataframe into sqlite
conn_db_py = create_engine('sqlite:///' + dir_data + 'CLEAN/db_api_report.sqlite')
df_bal.to_sql('tabl_RAW_BAL_API', conn_db_py)
df_pnl.to_sql('tabl_RAW_PNL_API', conn_db_py)
df_cash.to_sql('tabl_RAW_CASH_API', conn_db_py)
df_indi.to_sql('tabl_RAW_INDI_API', conn_db_py)



# ################# SINGEL QUAETER COLLECTION #################
#
# lst_bal = []
# lst_pnl = []
# lst_cash = []
# lst_indi = []
# for idx in list(df_sel.index.values):
#     json_bal = json.loads(get_data_post(url_api + 'p_stock2300', idx, token, '2017-12-31', 'json'))
#     json_pnl = json.loads(get_data_post(url_api + 'p_stock2301', idx, token, '2017-12-31', 'json'))
#     json_cash = json.loads(get_data_post(url_api + 'p_stock2302', idx, token, '2017-12-31', 'json'))
#     json_indi = json.loads(get_data_post(url_api + 'p_stock2303', idx, token, '2017-12-31', 'json'))
#     lst_bal.append(json_bal['records'][0]) if json_bal['resultmsg'] == 'success' and len(
#         json_bal['records']) >= 1 else ''
#     lst_pnl.append(json_pnl['records'][0]) if json_pnl['resultmsg'] == 'success' and len(
#         json_pnl['records']) >= 1 else ''
#     lst_cash.append(json_cash['records'][0]) if json_cash['resultmsg'] == 'success' and len(
#         json_cash['records']) >= 1 else ''
#     lst_indi.append(json_indi['records'][0]) if json_indi['resultmsg'] == 'success' and len(
#         json_indi['records']) >= 1 else ''
# ############################################################################################################
#
#
# ### prepare collected list into dataframe and save
# pd.DataFrame(lst_bal).to_csv(dir_data+'RAW/BAL.csv', encoding='utf-8')
# pd.DataFrame(lst_pnl).to_csv(dir_data+'RAW/PNL.csv', encoding='utf-8')
# pd.DataFrame(lst_cash).to_csv(dir_data+'RAW/CASH.csv', encoding='utf-8')
# pd.DataFrame(lst_indi).to_csv(dir_data+'RAW/INDI.csv', encoding='utf-8')

