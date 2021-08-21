module Http.Url exposing (batchGetItemURL, env, envTitle, getItemURL, prefixURL)


env : String
env =
    "prod"


envTitle : String
envTitle =
    "Prod"


baseURL : String
baseURL =
    "https://jskg4ocsd8.execute-api.us-west-2.amazonaws.com/" ++ env


batchGetItemURL : String
batchGetItemURL =
    baseURL ++ "/poli/batch-get-item"


getItemURL : String
getItemURL =
    baseURL ++ "/poli/get-item"


prefixURL : String
prefixURL =
    baseURL ++ "/poli/prefix/"
