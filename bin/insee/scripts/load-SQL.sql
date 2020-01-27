drop table if exists SQL_pays;

create table SQL_pays (
	Id INTEGER UNSIGNED auto_increment primary key,
	Iso2 CHAR(2) not null,
	Iso3 CHAR(3) not null,
	LibelleFr VARCHAR(100) not null
);

load data
 local infile 'COG/sql-pays.csv'
 ignore
 into table SQL_pays
 character set utf8
 fields terminated by ',' optionally enclosed by '"'
 ignore 1 rows
 (Id, @dummy, Iso2, Iso3, LibelleFr, @dummy)
;
