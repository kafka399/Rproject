git log master --shortstat --pretty="format: %ai"|sed -e 's/\+[0-9]*/,/g'|sed ':a;N;$!ba;s/ ,\n/,/g'|sed 's/ files changed//g'|sed 's/ insertions(,)//g'|sed 's/ deletions(-)//g' >gitstats.csv
