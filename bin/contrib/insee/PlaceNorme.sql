drop table if exists PlaceNorme;

create table PlaceNorme (
	Id INTEGER UNSIGNED auto_increment primary key,
	Code CHAR(5) not null,
	DateFin DATE not null,
	Libelle VARCHAR(500),
	index I_Place_Code (Code)
);

insert into PlaceNorme
select null, Code, '2050-01-01', Libelle from COG_pays;

insert into PlaceNorme
select null, c.Code, '2050-01-01', concat_ws(', ', c.Libelle, d.Libelle, if(length(d.Code)=3, null, 'France') )
from COG_commune c
inner join COG_departement d on c.Departement = d.Code
where c.TypeCom = "COM";

insert into PlaceNorme
select null, c.Code, '2050-01-01', concat_ws(', ', c.Libelle, n.Libelle)
from COG_commune c
inner join PlaceNorme n on c.Parent = n.Code
where c.TypeCom in ("COMA","COMD");

insert into PlaceNorme
select null, c.Code, '2050-01-01',
 concat(
 substr(c.Libelle, 1, locate(' ', c.Libelle)),
 lpad(
  substr(c.Libelle, locate(' ', c.Libelle)+1, locate('e', substr(c.Libelle, locate(' ', c.Libelle)+1))-1),
  2, '0'), ', ', n.Libelle )
from COG_commune c
inner join PlaceNorme n on c.Parent = n.Code
where TypeCom = "ARM";

insert into PlaceNorme
select null, m.AvCode, m.DateMvt, n.Libelle
from COG_mvt m
inner join PlaceNorme n on m.ApCode = n.Code
where TypeMvt = "41";

insert into PlaceNorme
select null, m.AvCode, m.DateMvt, concat(m.AvLibelle, ', ', n.Libelle)
from COG_mvt m
inner join PlaceNorme n on m.APCode = n.Code
where TypeMvt in ("31", "32", "33") and AvLibelle <> ApLibelle;
