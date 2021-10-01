# Détection en temps réels des points de retournement : apport de l’utilisation des filtres asymétriques dans l'analyse conjoncturelle

L’analyse du cycle économique, et en particulier la détection rapide des points de retournement d'une série, est un sujet de première importance dans l'analyse de la conjoncture économique. Les moyennes mobiles ou les filtres linéaires sont omniprésents dans les méthodes d'extraction du cycle économique et d'ajustement saisonnier. Ainsi, la méthode de désaisonnalisation X-12ARIMA utilise des moyennes mobiles de Henderson et des moyennes mobiles composites pour estimer les principales composantes d'une série chronologique. Au centre de la série, des filtres symétriques sont appliqués. En revanche, en raison du manque d’observations futures, pour estimer les points les plus récents, toutes ces méthodes doivent s'appuyer sur des filtres asymétriques. Par exemple, même si X-12ARIMA applique des moyennes symétriques aux prévisions obtenues à partir d'un modèle ARIMA, cela consiste en réalité à appliquer des filtres asymétriques en fin de série, car les valeurs prédites sont des combinaisons linéaires de valeurs passées. 

Si ces moyennes mobiles asymétriques ont de bonnes propriétés concernant la taille des révisions futures induites par le processus de lissage, elles induisent également des déphasages qui retardent en général la détection en temps réel des points de retournement.

Plusieurs travaux ont été menés sur l’estimation de la tendance-cycle en temps-réel. Parmi les méthodes non-paramétriques nous pouvons notamment citer :

- L’approche de Proietti et Luati (2008) qui considèrent le problème général de l'estimation de la tendance-cycle en temps réel au moyen de polynômes locaux et proposent une famille générale de filtres asymétriques qui minimisent la moyenne quadratique des révisions sous contraintes de conservation locale de polynômes ;

- L’approche de Dagum et Bianconcini (2008) qui modifient les filtres de Henderson en les projetant au préalable dans des espaces de Hilbert — Reproducing Kernel Hilbert Space (RKHS) ;

- L’approche de Grun-Rehomme, Guggemos, et Ladiray (2018) qui développent une procédure globale de construction de moyennes mobiles asymétriques basées sur l’optimisation sous contrainte de la somme pondérée de trois critères : fidélité (réduction du bruit), lissage et temporalité (mesure du déphasage).

L’objectif de ce stage sera d’étudier la possibilité d’établir une théorie générale sur la construction des moyennes mobiles qui englobe les méthodes citées précédemment.

Version web du rapport : https://aqlt-stage3a.netlify.app.
