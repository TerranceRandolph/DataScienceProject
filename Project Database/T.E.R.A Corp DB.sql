
/* 
			Terrance Randolph
             Final Project
               Proposal 1

*/
-----------------------------
--- Client Identity Table ---
-----------------------------
CREATE TABLE client_Identity (
	--Columns For The Client information Table 
	clientID int identity not null,
	client_FirstName varchar(30) not null,
	client_LastName varchar(30) not null,
	---
	CONSTRAINT PK_client_Identity PRIMARY KEY (clientID))


-------------------------
--- Client Demo Table ---
-------------------------
CREATE TABLE client_Demo (
	--Columns For The Client information Table 
	clientID int identity not null,
	Client_Age varchar(30) not null,
	Client_Kids int not null,
	Client_Race char(50),
	Client_Gender char(50),
	Client_Address varchar(30),
	---
	CONSTRAINT PK_client_Identity PRIMARY KEY (clientID))




----------------------------
--- Corp Specialty Table ---
----------------------------
CREATE TABLE Corp_Specialty_Client (
	--Columns For The Client Corp Table 
	clientID int identity not null,
	Corp_Business_Age int,
	Corp_Business_Type varchar(50),
	Filing_Status varchar(50)
	---
	CONSTRAINT PK_client_Identity PRIMARY KEY (clientID),
	CONSTRAINT FK_Corp_Specialty_Client FOREIGN KEY (Filing_Status) References Client_Info(Filing_Status))


-------------------------
--- Client Info Table ---
-------------------------
CREATE TABLE Client_Info (
	--Columns For The Client information Table 
	clientID int identity not null,
	Document_Retrieval_Method varchar(30),
	Refund_Amt int,
	Type_of_Filing char(50),
	Filing_Status varchar(50),
	Client_income int,
	Tax_Year int,
	Billed_Amt int,
	Payment_Plan int,
	Installment_Amt int,
	Date_of_Filing datetime not null
	---
	CONSTRAINT PK_clientInfo PRIMARY KEY (clientID),
	CONSTRAINT FK_Client_Info FOREIGN KEY (Filing_Status) References Corp_Specialty_Client(Filing_Status))

