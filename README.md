# migratum
[![Generic badge](https://img.shields.io/badge/STATUS-ALPHA-RED.svg)](https://shields.io/)

Migratum is a PostgreSQL migration utility.

```
Migratum: PostgreSQL Migration Tool

Usage: migratum COMMAND
  Migratum is a database tool that manages migrations

Available options:
  -h,--help                Show this help text

Available commands:
  new                      Generate necessary files for migration
  init                     Initialize database for migration
  migrate                  Perform Migration
```

# Initial Setup

### Database Connection
``` 
$ migratum new
```
This comment will generate the `migrations` directory which will contain
```
migrations
|-migratum.yaml
|-sql
```
`migratum.yaml` will contain the field below, which you can fill in with your 
database information.
```
config:
  postgres_password:
  postgres_db:
  postgres_user:
  postgres_host:
  postgres_port:
```
Once these fields have been filled in, perform this command

``` 
$ migratum init
```
This will create a table called `schema_migrations`. This table will store the 
information about the migrations that have been executed in the database

## Migration
Write your migration file in the `sql` directory with this convention.
```
V<version number>__<name of migration>.sql
```
For example:
```
V1__uuid_extension.sql
```
After you are done writing your migration script, perform this command

```
$ migratum migrate
```
You should see this response in your terminal
```
[Info]: MigrationPerformed V1__uuid_extension.sql
```
