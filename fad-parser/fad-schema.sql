
--
-- Table structure for table "classifications"
--

CREATE TABLE "classifications" (
  "id" integer NOT NULL,
  PRIMARY KEY ("id")
);


INSERT INTO "classifications" VALUES (1),(2),(3),(4),(5),(7),(8),(9),(10),(11),(12),(13),(15),(16),(18),(19),(20),(21),(25),(36);

--
-- Table structure for table "ingroup"
--

DROP TABLE IF EXISTS "ingroup";
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE "ingroup" (
  "qtr" integer ,
  "subject" varchar(128) ,
  "coursenum" varchar(10) ,
  "section" integer ,
  "groupnum" integer ,
  CONSTRAINT "ingroup_ibfk_1" FOREIGN KEY ("qtr", "subject", "coursenum", "section") REFERENCES "offerings" ("qtr", "subject", "num", "section")
);

-- Table structure for table "instructors"
--

DROP TABLE IF EXISTS "instructors";
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE "instructors" (
  "id" varchar(128) NOT NULL,
  "name" varchar(128) ,
  "soc" varchar(9) ,
  PRIMARY KEY ("id")
);

--
-- Dumping data for table "instructors"
--

DROP TABLE IF EXISTS "instructorstatuses";
CREATE TABLE "instructorstatuses" (
  "id" varchar(128) NOT NULL,
  "qtr" integer NOT NULL,
  "homedept" varchar(128) ,
  "tsf" integer ,
  "iaf" integer ,
  "osf" integer ,
  "admlevel" varchar(128) ,
  "rank" varchar(128) ,
  PRIMARY KEY ("id","qtr"),
  CONSTRAINT "instructorstatuses_ibfk_1" FOREIGN KEY ("id") REFERENCES "instructors" ("id"),
   CONSTRAINT "instructorstatuses_ibfk_3" FOREIGN KEY ("homedept") REFERENCES "depts" ("id")
);


-- Table structure for table "offerings"
--

DROP TABLE IF EXISTS "offerings";
CREATE TABLE "offerings" (
  "qtr" integer NOT NULL,
  "subject" varchar(128) NOT NULL,
  "num" varchar(10) NOT NULL,
  "section" integer NOT NULL,
  "discipline" varchar(10)  NOT NULL,
  "level" varchar(2) NOT NULL,
  "enrollment" integer  NOT NULL,
  "classification" integer,
  "accu" integer  NOT NULL,
  "groupcode" integer,
  PRIMARY KEY ("qtr","subject","num","section"),
  CONSTRAINT "offerings_ibfk_2" FOREIGN KEY ("classification") REFERENCES "classifications" ("id")
);

--
-- Table structure for table "offerfacs"
--

DROP TABLE IF EXISTS "offerfacs";
CREATE TABLE "offerfacs" (
  "qtr" integer NOT NULL,
  "subject" varchar(128) NOT NULL,
  "num" varchar(10) NOT NULL,
  "section" integer NOT NULL,
  "instructor" varchar(128) NOT NULL,
  "scu" integer NOT NULL,
  "fch" integer NOT NULL,
  "dwtu" integer NOT NULL,
  PRIMARY KEY ("qtr","subject","num","section","instructor"),
  CONSTRAINT "offerseqs_ibfk_1" FOREIGN KEY ("qtr", "subject", "num", "section") REFERENCES "offerings" ("qtr", "subject", "num", "section"),
  CONSTRAINT "offerseqs_ibfk_2" FOREIGN KEY ("instructor") REFERENCES "instructors" ("id")
);


--
-- Table structure for table "offerseqs"
--

DROP TABLE IF EXISTS "offerseqs";
CREATE TABLE "offerseqs" (
  "qtr" integer NOT NULL,
  "subject" varchar(128) NOT NULL,
  "num" varchar(10) NOT NULL,
  "section" integer NOT NULL,
  "sequence" integer NOT NULL,
  "instructor" varchar(128) NOT NULL,
  "ttf" integer NOT NULL,
  "days" varchar(10) ,
  "start" varchar(10) ,
  "end" varchar(10) ,
  "tba" integer ,
  "facility" varchar(10) ,
  "space" varchar(10) ,
  "facltype" integer ,
  PRIMARY KEY ("qtr","subject","num","section","sequence","instructor"),
  CONSTRAINT "offerseqs_ibfk_1" FOREIGN KEY ("qtr", "subject", "num", "section","instructor") REFERENCES "offerfacs" ("qtr", "subject", "num", "section","instructor"),
);

--
-- Table structure for table "specialcredits"
--

DROP TABLE IF EXISTS "specialcredits";
CREATE TABLE "specialcredits" (
  "id" integer NOT NULL AUTO_INCREMENT,
  "instructor" varchar(128) ,
  "qtr" integer ,
  "dwtu" integer ,
  "iwtu" integer ,
  "description" varchar(128) ,
  "description" varchar(128) DEFAULT NULL,
  PRIMARY KEY ("id"),
  KEY "instructor" ("instructor"),
  KEY "qtr" ("qtr"),
  CONSTRAINT "specialcredits_ibfk_1" FOREIGN KEY ("instructor") REFERENCES "instructors" ("id"),
  CONSTRAINT "specialcredits_ibfk_2" FOREIGN KEY ("qtr") REFERENCES "quarters" ("id")
);

--
-- Dumping data for table "specialcredits"
--

DROP TABLE IF EXISTS "tenuretracks";
CREATE TABLE "tenuretracks" (
  "id" varchar(128) NOT NULL,
  PRIMARY KEY ("id"),
  CONSTRAINT "tenuretracks_ibfk_1" FOREIGN KEY ("id") REFERENCES "instructors" ("id")
);

--
-- Dumping data for table "tenuretracks"
--

-- This is seriously incomplete at this point...
INSERT INTO "tenuretracks" VALUES ('A   KEEN'),('A   NAFISI'),('A   PANDE'),('A   SHABAN'),('A A LIDDICOAT'),('A E CHATZIIOANOU'),('A I DAVOL'),('A J KEAN'),('A K SEIFODDINI'),('A M DEKHTYAR'),('A M RAHIM'),('A R POURAGHABAGHER'),('B   QU'),('B D LONDON'),('B G BENSON'),('B J MEALY'),('B P SELF'),('C   LO'),('C   LUPO'),('C   SUN'),('C A MACCARLEY'),('C A STALEY'),('C B BIRDSONG'),('C B CHADWELL'),('C C PASCUAL'),('C K POKORNY'),('C M CLARK'),('C S TURNER'),('D B BRAUN'),('D C JANSEN'),('D D MARSHALL'),('D E WHITE'),('D I KACHLAKEV'),('D J BIEZAD'),('D J DERICKSON'),('D J DETURRIS'),('D J WALDORF'),('D S CLAGUE'),('D S DOLAN'),('D S JANZEN'),('D W WALSH'),('D Y ARAKAKI'),('E A MEHIEL'),('E C SULLIVAN'),('E P KASPER'),('F   DEPIERO'),('F   KHOSMOOD'),('F A KOLKAILAH'),('F C OWEN'),('F J KURFESS'),('G A GRANNEMAN'),('G E THORNCROFT'),('G J HALL'),('G L FIEGEL'),('G L FISHER'),('G T MASE'),('H   GHARIBYAN'),('H E GASCOIGNE'),('H M COTA'),('H M SMITH'),('H V PORUMAMILLA'),('I E VAKALIS'),('J   MADDREN'),('J   PAN'),('J   PUIG-SUARI'),('J   TSO'),('J A MACEDO'),('J A SAGHRI'),('J B CLEMENTS'),('J B CONNELY'),('J C CHEN'),('J D MELLO'),('J E GRIMES'),('J G HARRIS'),('J G LOCASCIO'),('J L HANSON'),('J M BELLARDO'),('J M MEAGHER'),('J M WIDMANN'),('J R BREITENBACH'),('J R RIDGELY'),('J S DENATALE'),('J S SENG'),('J Y OLIVER'),('K A SHOLLENBERGER'),('K C CHEN'),('K J ABERCROMBY'),('K K JAMESON'),('K O CARDINAL'),('K W COLVIN'),('L A SLIVOVSKY'),('L H LAIHO'),('L S VANASUPA'),('L T SCHLEMER'),('L V GRIFFIN'),('M   LIU'),('M   NOORI'),('M K MULETA'),('M L HAUNGS'),('M M CIROVIC'),('M M MEDIZADE'),('N   PAL'),('N   TAUFIK'),('P   LEMIEUX'),('P E RAINEY'),('P J SCHUSTER'),('P L NICO'),('R   JAVADPOUR'),('R A MCDONALD'),('R B SZLAVIK'),('R E MOSS'),('R K GOEL'),('R N SAVAGE'),('R S CROCKETT'),('R S SANDIGE'),('R V WESTPHAL'),('S   MITRA'),('S   MOAZZAMI'),('S   NIKU'),('S   RAHMAN'),('S A VIGIL'),('S C TANDON'),('S E ALPTEKIN'),('S J HAZELWOOD'),('S L HOCKADAY'),('S M KLISCH'),('S O AGBO'),('T   FREED'),('T   YANG'),('T H SMILKSTEIN'),('T J KEARNS'),('T J LUNDQUIST'),('T J MACKIN'),('T L THATCHER'),('T R CARDINAL'),('T S HARDING'),('U   MENON'),('V I PRODANOV'),('W C BUCKALEW'),('W C PILKINGTON'),('W L AHLGREN'),('W R MURRAY'),('W W DURGIN'),('X   JIN'),('X   WU'),('X   ZHANG'),('X H YU'),('Y C YONG'),('Y M NELSON'),('Z J WOOD');


CREATE TABLE "depts" (
  "id" varchar(128) NOT NULL,
  "fullname" varchar(128) NOT NULL,
  PRIMARY KEY ("id")
);

NSERT INTO "depts" VALUES ('AERO','112 AERO ENG'),('ALLSCHOOL','132 ALL SCHOOL'),('BMGE','224 BIOMEDICAL ENGINEERING'),('CEENVE','176 CIVIL/ENV ENG'),('CSC','189 COMPUTER SCIENCE'),('EE','247 ELECTRICAL ENGINEERING'),('IME','363 IND ENG'),('MATE','770 WELDING AND METALLURGICAL ENGINEERING'),('ME','490 MECHANICAL ENG');


-- Data redesign notes
-- * represent "home department" as a separate table.
-- * each sequence/faculty line is a separate record.


