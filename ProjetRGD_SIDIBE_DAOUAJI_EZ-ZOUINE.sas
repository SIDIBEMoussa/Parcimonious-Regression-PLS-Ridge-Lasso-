/* Travail fait par :
-DAOUAJI Soukaina
-EZ-ZOUINE Amina
-SIDIBE Moussa */

FILENAME REFFILE DISK '/shared/home/msidibe@insea.ac.ma/Projet_RGD/Donnees_projet.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=BIOBLANC REPLACE;
	DELIMITER=',';
	GETNAMES=YES;
RUN;

/* Afficher la statistique descriptive sur le dataset bioblanc */
PROC MEANS DATA=BIOBLANC;
VAR Y X1 X2 X3 X4 X5 X6 X7 X8;
RUN;

/* Affiche des boxplots sur les variables d'origines Y X1 X2 X3*/
PROC SORT DATA=BIOBLANC;
BY X1;
PROC BOXPLOT DATA=BIOBLANC;
PLOT X2*X1;

PROC BOXPLOT DATA=BIOBLANC;
PLOT Y*X1;

PROC SORT DATA=BIOBLANC;
BY X2;

PROC BOXPLOT DATA=BIOBLANC;
PLOT Y*X2;

PROC SORT DATA=BIOBLANC;
BY X3;

PROC BOXPLOT DATA=BIOBLANC;
PLOT Y*X3;

/* Calcul de la matrice de correlation */
PROC CORR DATA=BIOBLANC;
TITLE "Matrice de corrélation";
VAR Y X1 X2 X3;
RUN;

/* Afficher la dispersion des variables "l'une autour des autres" */
PROC SGSCATTER DATA=BIOBLANC;
MATRIX Y X1 X2 X3;
TITLE "DIAGRAMME DE DISPERSION";
RUN;

/* Dans cette partie nous nous intéressons à l'étude des régressions simples */

/*On commence par étudier la corrélation entre variables*/
PROC CORR DATA=BIOBLANC;
TITLE "Matrice de corrélation";
VAR Y X1 X2 X3 X4 X5 X6 X7 X8;
RUN;


PROC sgscatter DATA=BIOBLANC;
TITLE "Dispersion des variables par rapport à Y";
COMPARE X=Y Y=(X1 X2 X3 X4 X5 X6 X7 X8);
RUN;
Proc reg data=Work.bioblanc;
model Y= X1 /R INFLUENCE;
title "La régression de la vente Y sur X1";
run;

Proc reg data=Work.bioblanc;
model Y= X2 /R INFLUENCE;
title "La régression de la vente Y sur X2";
run;

Proc reg data=Work.bioblanc;
model Y= X3 /R INFLUENCE;
title "La régression de la vente Y sur X3";
run;

Proc reg data=Work.bioblanc;
model Y= X4 /R INFLUENCE;
title "La régression de la vente Y sur X4";
run;

Proc reg data=Work.bioblanc;
model Y= X5 /R INFLUENCE;
title "La régression de la vente Y sur X5";
run;

Proc reg data=Work.bioblanc;
model Y= X6 /R INFLUENCE;
title "La régression de la vente Y sur X6";
run;

Proc reg data=Work.bioblanc;
model Y= X7 /R INFLUENCE;
title "La régression de la vente Y sur X7";
run;

Proc reg data=Work.bioblanc;
model Y= X8 /R INFLUENCE;
title "La régression de la vente Y sur X8";
run;

/* La régression de Y sur toutes les régresseurs de la base des données*/

Proc reg data=Work.bioblanc;
model Y= X1 X2 X3 X4 X5 X6 X7 X8/R INFLUENCE;
title "La régression de la vente Y sur X1 X2 X3 X4 X5 X6 X7 X8";
run;

/*Avec cette procedure nous tentons de supprimer la multicolinéarité
dans notre modèle mais à la fin nous remarquons qu'il y a une incohérence au niveau des
signes des coefficients de régression car cette selection s'éffectue sequentiellement
et le choix est au niveau individuel
*/

Proc reg data=Work.bioblanc;
model Y= X1 X2 X3 X4 X5 X6 X7 X8/ selection=Backward;
title "La régression de la vente Y sur X1 X2 X3 X4 X5 X6 X7 X8";
run;

/*Enfin nous nous intéressons à l'objectif de ce projet 
en appliquant la méthode de Partial Least Square regression*/

/* Noramlisation des données pour supprimer l'effet d'échelle*/

proc standard data=Work.bioblanc mean=0 std=1 
out= Bioblanc_stan;
var Y X1 X2 X3 X4 X5 X6 X7 X8;
run;

/*Cette méthode a pour objectif de maximiser la corrélation entre les variables construites 
et la variable cible Y tout en réduisant la dimension avec la combinaison linéaire des variables
originales. Ainsi, nous nous basons sur les variables significativement corrélées avec la vente Y pour
contruire T1(Première variable latente)
*/
proc corr data=work.bioblanc;
title "Matrice de corrélation";
var Y X1 X2 X3 X4 X5 X6 X7 X8;
run;

/* Contruction de T1*/
/*
T1=(-0.43448*X1+0.69050*X2+0.87446*X3+0.88516*X4+0.89022*X5+0.88730*X6+0.81358*X7+0.88908*X8)
/(0.43448^2+0.69050^2+0.87446^2+0.88516^2+0.89022^2+0.88730^2+0.81358^2+0.88908^2)
5.092678944899999=(0.47528^2+0.72867^2+0.87037^2+0.88041^2+0.88192^2+0.88053^2+0.83469^2+0.88274^2)
*/

proc sql;
create table table1 as 
(select ID,  -0.47528*X1+0.72867*X2+0.87037*X3+0.88041*X4+0.88192*X5+0.88053*X6+0.83469*X7+0.88274*X8 as nume from work.bioblanc_stan);
create table table2 as 
(select ID, nume/sqrt(5.3185700232999995) as T1 from work.table1);
run;

/*On observe si les modifications ont été faites*/

proc print data=table2; run;

/*On rassemble la table contenant les variables standardisées
 avec celle contenant les valeurs de T1 pour les observations
 dans une table nommée final_table afin de l'utiliser 
pour les operations suivantes*/

proc sql;
create table final_table as 
select * from table2 natural join bioblanc_stan;
run;

proc print data=final_table;

/*Comme cité ci-dessus nous cherchons le minimum de variables possibles pour contruire notre modèle
mais avec qui soit robuste, ainsi, ayant déjà construit T1 on s'intéresse à construire une autre composante à partir des résidus
qui paraissent très grands ou significatifs pour apporter des informations supplémentaires pour expliquer Y.
Nous éffectuons alors ces régressions sur les Xi sachant T1 pour repérer les variables significatives
*/

/*On ne retient pas X1 pour construire T2*/

proc reg data=final_table;
model Y=T1 X1;
Title "Regression de Y sur X1 sachant T1";
run;

/* On ne retient pas X2 pour construire T2*/

proc reg data=final_table;
model Y=T1 X2;
Title "Regression de Y sur X2 sachant T1";
run;

/* On ne retient pas X3 pour construire T2*/

proc reg data=final_table;
model Y=T1 X3;
Title "Regression de Y sur X3 sachant T1";
run;

/* On ne retient pas X4 pour construire T2*/

proc reg data=final_table;
model Y=T1 X4;
Title "Regression de Y sur X4 sachant T1";
run;

/* On ne retient pas X5 pour construire T2*/

proc reg data=final_table;
model Y=T1 X5;
Title "Regression de Y sur X5 sachant T1";
run;

/* On ne retient pas X6 pour construire T2*/

proc reg data=final_table;
model Y=T1 X6;
Title "Regression de Y sur X6 sachant T1";
run;

/* On retient X7 pour construire T2*/

proc reg data=final_table;
model Y=T1 X7;
Title "Regression de Y sur X7 sachant T1";
run;

/*On ne retient pas X8 pour construire T2*/

proc reg data=final_table;
model Y=T1 X8;
Title "Regression de Y sur X8 sachant T1";
run;

/*Pour la prochaine étape nous ne retenons que X7 car elle est la seule qui présente des 
une part d'information importante non emmagasinée par T1, alors nous récupererons la part non emmagasinée
par T1 à travers son résidu de la regression de X7 sur T1
*/

/*Dans cette partie nous procédons à la récupération
du rédidu de la régression de X7 sur T1*/

proc reg data=Work.final_table;
model X7= T1/R Influence;
output out=final_table residual=residual17;
run;

proc print data=final_table; run;

/*Nous normalisons ce résidu et l'ajoutons au final_table*/

proc sql;
create table final_table as
select *, 
residual17/var(residual17) as residual17_cn
from final_table;


proc print data=final_table;
run;


/*Nous confirmons que le résidu est significatif sachant T1 dans l'explication de Y*/

/*residual17_cn est significative à 95% de confiance*/

proc reg data=final_table;
model Y=T1 residual17_cn;
run;

/*A partir de ce résidu nous contruisons la deuxième composante T2 comme suit*/

/*Y=beta0+beta1*T1+beta2*residual17_cn et beta2=-0.04746*/

proc sql;
create table table as
(select ID,-0.04746*residual17 as nume2
from final_table);
create table table as 
(select ID, nume2/0.04746 as T2 from table);

proc print data=table; run;

/*On l'ajoute dans la table final_table*/

proc sql;
create table final_table as
select * from table natural join final_table;

proc print data=final_table; run;

/* Nous cherchons une troisième composante permettant d'apporter encore une information
supplémentaire mais au final aucune variable n'apporte de nouvelles informations
*/

/*X1 ne rentre pas dans la construction de T3*/

proc reg data=final_table;
title "La régression de Y sur X1 sachant T1 et T2";
model Y=T1 T2 X1/R influence;
run;

/*X2 ne rentre pas dans la construction de T3*/

proc reg data=final_table;
title "La régression de Y sur X2 sachant T1 et T2";
model Y=T1 T2 X2/R influence;
run;

/*X3 ne rentre pas dans la construction de T3*/

proc reg data=final_table;
title "La régression de Y sur X3 sachant T1 et T2";
model Y=T1 T2 X3/R influence;
run;

/*X4 ne rentre pas dans la construction de T3*/

proc reg data=final_table;
title "La régression de Y sur X4 sachant T1 et T2";
model Y=T1 T2 X4/R influence;
run;

/*X5 ne rentre pas dans la construction de T3*/

proc reg data=final_table;
title "La régression de Y sur X5 sachant T1 et T2";
model Y=T1 T2 X5/R influence;
run;

/*X6 ne rentre pas dans la construction de T3*/

proc reg data=final_table;
title "La régression de Y sur X6 sachant T1 et T2";
model Y=T1 T2 X6/R influence;
run;

/*X7 rentre pas dans la construction de T3*/

proc reg data=final_table;
title "La régression de Y sur X7 sachant T1 et T2";
model Y=T1 T2 X7/R influence;
run;

/*X8 ne rentre pas dans la construction de T3*/

proc reg data=final_table;
title "La régression de Y sur X8 sachant T1 et T2";
model Y=T1 T2 X8/R influence;
run;

/* Aucune des variables X1 ...... X8 n'est significative au niveau de risque 5%
il faut retenir que les deux composantes pls T1 et T2*/

proc print data=final_table;	run;

/*Enfin, nous récuperons dans la table_utile les variables dont on aura besoin pour construire le modèle, à savoir Y, T1 et T2*/

proc sql;
create table temp1 as select ID,T1,T2 from final_table;
create table temp2 as select ID,Y from bioblanc;
create table table_utile as select * from temp1 natural join temp2;
run;

/* La regréssion multiple de la vente sur les deux variables latentes T1 et T2 */

proc reg data=table_utile;
title "La régression de Y sur T1 et T2";
model Y=T1 T2/R influence;
run;

proc print data=table_utile ; run;

/*Nous appliquons la méthode de l'ACP pour confirmer le résultat des deux composantes retenues, en utilisant seulement les 2 composantes nous
pouvons expliquer plus de 94% de l'inertie totales de la population*/

PROC PRINCOMP DATA=WORK.bioblanc_stan N=4 out=coordon
 plots=all;
var X1 X2 X3 X4 X5 X6 X7 X8;
run;

/*On valide encore une fois les résultats obtenus avec pls automatisé*/

ods graphics on;
proc pls data=work.bioblanc_stan plots=(ParmProfiles VIP) cv=block;
model Y = X1 X2 X3 X4 X5 X6 X7 X8;
run;
ods graphics off;
