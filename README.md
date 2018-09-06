# hadrianus


SELECT Customer, D.Date, Location.Lat as Latitude, Location.Long as Longitude, SUM([Total Excluding Tax]) AS [Total Excluding Tax], SUM([Total Including Tax]) AS [Total Including Tax], SUM(Profit) AS Profit, Employee AS SalesPerson
FROM Fact.Sale S JOIN Dimension.Customer C ON S.[Customer Key] = C.[Customer Key]
	JOIN Dimension.Employee E ON S.[Salesperson Key] = E.[Employee Key]
	JOIN Dimension.City Ci on S.[City Key]=Ci.[City Key]
	JOIN Dimension.Date D ON S.[Invoice Date Key]=D.Date
GROUP BY Customer, D.Date, Location.Lat, Location.Long, Employee
