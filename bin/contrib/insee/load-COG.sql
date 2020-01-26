drop table if exists COG_pays;
drop table if exists COG_departement;
drop table if exists COG_commune;
drop table if exists COG_mvt;

create table COG_pays (
	Id INTEGER UNSIGNED auto_increment primary key,
	Code CHAR(5) not null,
	Actual ENUM( "1", "2", "3", "4") not null,
	PaysRatAncien CHAR(5),
	PaysRatActuel CHAR(5),
        Independance CHAR(4),
	Libelle VARCHAR(100) not null,
	LibelleEnr VARCHAR(100) not null,
	AncienNom VARCHAR(100),
	Iso2 CHAR(2) not null,
	Iso3 CHAR(3) not null,
	UNIQUE (Code)
);

load data
 local infile 'COG/COG-pays.csv'
 ignore
 into table COG_pays
 character set utf8
 fields terminated by ',' optionally enclosed by '"'
 ignore 1 rows
 (Code, Actual, @rtAnc, @ratAct, @ind, Libelle, LibelleEnr, @ancien, Iso2, Iso3, @dummy)
 set Id = null,
     PaysRatAncien = nullif(@rtAnc, ''),
     PaysRatActuel = nullif(@rtAct, ''),
     Independance = nullif(@ind, ''),
     AncienNom = nullif(@ancien, '')
;

update COG_pays
inner join SQL_pays using(Iso3)
set Libelle = LibelleFr;

create table COG_departement (
	Id INTEGER UNSIGNED auto_increment primary key,
	Code CHAR(3) not null,
	Libelle VARCHAR(100) not null,
	UNIQUE (Code)
);

load data
 local infile 'COG/COG-departement.csv'
 ignore
 into table COG_departement
 character set utf8
 fields terminated by ',' optionally enclosed by '"'
 ignore 1 rows
 (Code, @dummy, @dummy, @dummy, @dummy, @dummy, Libelle)
 set Id = null
;

create table COG_commune (
	Id INTEGER UNSIGNED auto_increment primary key,
	Code CHAR(5) not null,
	TypeCom ENUM( "COM", "COMA", "COMD", "ARM" ) not null,
	Departement CHAR(3) default null,
	Libelle VARCHAR(100) not null,
	Parent CHAR(5) default null,
	UNIQUE (Code)
);

load data
 local infile 'COG/COG-commune.csv'
 ignore
 into table COG_commune
 character set utf8
 fields terminated by ',' optionally enclosed by '"'
 ignore 1 rows
 (TypeCom, Code, @dummy, @dep, @dummy, @dummy, @dummy, @dummy, Libelle, @dummy, @parent)
 set Id = null,
     Departement = nullif(@dep, ''),
     Parent = nullif(@parent, '')

;

create table COG_mvt (
	Id INTEGER UNSIGNED auto_increment primary key,
	TypeMvt ENUM ( "10", "20", "21", "30", "31", "32", "33", "34", "41", "50", "70") not null,
	DateMvt DATE not null,
	AvTypeCom ENUM( "COM", "COMA", "COMD", "ARM" ) not null,
	AvCode CHAR(5) not null,
	AvLibelle VARCHAR(100) not null,
	ApTypeCom ENUM( "COM", "COMA", "COMD", "ARM" ) not null,
	ApCode CHAR(5) not null,
	ApLibelle VARCHAR(100) not null
);

load data
 local infile 'COG/COG-mvt.csv'
 ignore
 into table COG_mvt
 character set utf8
 fields terminated by ',' optionally enclosed by '"'
 ignore 1 rows
 (TypeMvt, DateMvt, AvTypeCom, @cod1, @dummy, @dummy, @dummy, AvLibelle, ApTypeCom, @cod2, @dummy, @dummy, @dummy, ApLibelle)
 set Id = null,
     AvCode = lpad( @cod1, 5, '0' ),
     ApCode = lpad( @cod2, 5, '0' )
;
