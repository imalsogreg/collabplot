HOST=$1
USER=collabplot
PW=$3
DB=collabplot

createuser $USER
psql -U postgres -c "ALTER ROLE $USER WITH PASSWORD '$PW';"
dropdb $DB
createdb $DB
psql -U postgres -d $DB -c "CREATE EXTENSION \"uuid-ossp\";"
psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE $DB TO $USER;"

psql -h $HOST -p 5432 -U $USER -d $DB -f 'create.sql'
psql -h $HOST -p 5432 -U $USER -d $DB -f 'sampledata.sql'
