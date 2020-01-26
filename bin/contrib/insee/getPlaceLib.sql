drop function if exists insee.getPlaceLib;

delimiter //
create function insee.getPlaceLib(
 inCode CHAR(5),
 inEffetY CHAR(4),
 inEffetM CHAR(2),
 inEffetD CHAR(2)
)
RETURNS VARCHAR(500)
BEGIN
	DECLARE res VARCHAR(500);

	select Libelle into res
	from PlaceNorme
	where Code = inCode
	  and concat_ws('-', inEffetY,inEffetM,inEffetD) < DateFin
	order by DateFin asc
	limit 1;
	return( res );
END//
delimiter ;
