HOST=$1
USER=$2
PW=$3
DB=collabs

sudo -u postgres dropuser $USER
sudo -u postgres createuser $USER
sudo -u postgres psql -c "ALTER ROLE $USER WITH PASSWORD '$PW';"
sudo -u postgres dropdb $DB
sudo -u postgres createdb $DB
sudo -u postgres psql -d $DB -c "CREATE EXTENSION \"uuid-ossp\";"
sudo -u postgres psql -c "GRANT ALL PRIVILEGES ON DATABASE $DB TO $USER;"

psql -h $HOST -p 5432 -U $USER -d $DB -f 'create.sql'
psql -h $HOST -p 5432 -U $USER -d $DB -f 'sampledata.sql'
