#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

typedef struct AFD {
    int taille_Q;
    int taille_Sigma;
    int q0;
    bool* finaux;
    int** delta;
}afd;


void liberer_afd(afd* A)
{
    free(A->finaux);
    for (int q=0; q<A->taille_Q; q++)
    {
        free(A->delta[q]);
    }
    free(A->delta);
    free(A);
}

afd* initialiser_afd(int taille_Q, int taille_Sigma, int q0)
{
    afd* A = malloc(sizeof(afd));
    A->taille_Q = taille_Q;
    A->taille_Sigma = taille_Sigma;
    A->q0 = q0;
    bool* finaux = malloc(taille_Q * sizeof(bool));
    int** delta = malloc(taille_Q * sizeof(int*));
    int* tab;
    //On initialise au cours de la même boucle les cases de finaux et les tableaux de delta.
    for (int q=0; q<taille_Q; q++)
    {
        finaux[q] = false;
        tab = malloc(taille_Sigma * sizeof(int));
        for (int a=0; a<taille_Sigma; a++)
        {
            tab[a] = -1;
        }
        delta[q] = tab;
    }
    A->finaux = finaux;
    A->delta = delta;
    return A;
}

int conversion_lettre(char c)
{
    return ((int) c) -97;
}

void ajout_transition_afd(afd* A, int q, char a, int p)
{
    A->delta[q][conversion_lettre(a)] = p;
}

int delta_etoile_afd(afd* A, int q, char* u)
{
    int etat_courant = q;
    int i = 0;
    while (u[i] != '\0' && etat_courant != -1)
    //On lit u jusquà  trouver un blocage ou le symbole de fin de chaîne.
    {
        etat_courant = A->delta[etat_courant][conversion_lettre(u[i])];
        i++;
    }
    return etat_courant;
}

bool reconnu_afd(afd* A, char* u){
    int etat_arrivee = delta_etoile_afd(A, A->q0, u);
    if (etat_arrivee == -1)
    {
        //cas d'un blocage
        return false;   
    }
    return A->finaux[etat_arrivee];
}

/* Fonctions d'affichage */

void affiche_tableau(int* tab, int n)
{
    for (int i = 0 ; i < n ; i++)
    {
        printf("%d ", tab[i]);
    }
    printf("\n");
}


void affiche_tableau_bool(bool* tab, int n)
{
    for (int i = 0 ; i < n ; i++)
    {
        printf("%d ", tab[i]);
    }
    printf("\n");
}

void affiche_matrice(int**m, int l, int c)
{
    for (int i =0; i < l; i++)
    {
        affiche_tableau(m[i],c);
    }
}






int* initialiser_classes(afd *a){
    int n = a->taille_Q;
    int f; int r; //f = couleur des sommets finaux et r = couleur du reste
    if (a->finaux[0]){
        f = 0;
        r = 1;
    }
    else {
        f = 1;
        r = 0;
    }
    int* classes = malloc(sizeof(int)*n);
    for (int i=0; i<n; i++){
        if (a->finaux[i]){
            classes[i] = f;
        }
        else {
            classes[i] = r;
        }
    }
    return classes;
}

int classe_d_arrivee(afd* a, int* c, int q, int l){
    return c[a->delta[q][l]];
}

bool distinguables(afd* a, int* c, int q, int r){
    if (c[q] != c[r]){
        return true;
    }
    int nb_lettres = a->taille_Sigma;
    for (int l=0; l<nb_lettres; l++){
        if (classe_d_arrivee(a, c, q, l) != classe_d_arrivee (a, c, r, l)){
            return true; //q et r ne sont pas dans la meme classe pour k+1
        }
    }
    return false; //q et r sont dans la meme classe pour k+1
}

int* raffiner(afd* a, int* c){
    int n = a->taille_Q;
    int* new_c = malloc(sizeof(int)*n);
    int nb_cl = 0;
    int* repr = malloc(sizeof(int)*n);
    for (int i=0; i<n; i++){
        repr[i] = -1;
    }
    for (int i=0; i<n; i++){
        bool nv_classe = true;
        int couleur = -1;
        for (int j=0; j<nb_cl && nv_classe; j++){ //on regarde si le sommet i est dans une classe
            assert(repr[j] != -1);                //déjà attribuée
            nv_classe = distinguables(a, c, i, repr[j]);
            if (!nv_classe){
                couleur = j;
            }
        }
        if (nv_classe){ //sinon il fait partie d'une nouvelle classe
            repr[nb_cl] = i;
            new_c[i] = nb_cl;
            nb_cl ++;
        }
        else {
            new_c[i] = couleur;
        }
    }
    return new_c;
}

bool different(int* tab1, int* tab2, int len){
    //renvoie false si les deux tableaux sont égaux (ont le même contenu)
    for(int i=0; i<len; i++){
        if (tab1[i] != tab2[i]){
            return true;
        }
    }
    return false;
}

int* nerode(afd* a){
    int n = a->taille_Q;
    int* ck = initialiser_classes(a);
    int* ckp1 = raffiner(a, ck);
    while(different(ck, ckp1, n)){
        free(ck);
        ck = ckp1;
        ckp1 = raffiner(a, ck);
    }
    return ckp1;
}

afd* minimiser(afd* a){
    int n = a->taille_Q;
    int* classes = nerode(a);
    int* repr = malloc(sizeof(int)*n);
    for (int i=0; i<n; i++){
        repr[i] = -1;
    }
    int nb_c = 0;
    for (int i=0; i<n; i++){
        if (classes[i] == nb_c){
            repr[nb_c] = i;
            nb_c ++;
        }
    }
    //creation de l'automate minimal
    afd* new_a = malloc(sizeof(afd));
    new_a->taille_Q = nb_c;
    new_a->q0 = classes[a->q0];
    new_a->taille_Sigma = a->taille_Sigma;
    //nouveau delta
    int** new_d = malloc(sizeof(int*)*nb_c);
    for (int i=0; i<nb_c; i++){
        int* ligne = malloc(sizeof(int)*a->taille_Sigma);
        new_d[i] = ligne;
        for (int l=0; l<a->taille_Sigma; l++){
            new_d[i][l] = classe_d_arrivee(a, classes, repr[i], l);
        }
    }
    new_a->delta = new_d;
    //nouveau finaux
    int i=0;
    while(!a->finaux[i]){
        i++;
    }
    bool* new_finaux = malloc(sizeof(bool)*nb_c);
    for (int j=0; j<nb_c; j++){
        new_finaux[j] = false;
    }
    new_finaux[classes[i]] = true;
    new_a->finaux = new_finaux;
    return new_a;
}



int nombre_classes(int* c, int len){
    int max = 0;
    for (int i=0; i<len; i++){
        if (c[i] > max){
            max = c[i];
        }
    }
    return max + 1;
}


int main()
{
    afd* a = initialiser_afd(6,2,0);
    a->finaux[5] = true;
    ajout_transition_afd(a,0,'a',1); ajout_transition_afd(a,0,'b',3);
    ajout_transition_afd(a,1,'a',2); ajout_transition_afd(a,1,'b',5);
    ajout_transition_afd(a,2,'a',2); ajout_transition_afd(a,2,'b',5);
    ajout_transition_afd(a,3,'a',4); ajout_transition_afd(a,3,'b',5);
    ajout_transition_afd(a,4,'a',4); ajout_transition_afd(a,4,'b',5);
    ajout_transition_afd(a,5,'a',5); ajout_transition_afd(a,5,'b',5);
    printf("a reconnait abb : %d \n", reconnu_afd(a,"abb"));
    printf("a reconnait aaa : %d \n", reconnu_afd(a,"aaa"));

    
    int* c = initialiser_classes(a);
    int nb_classes = nombre_classes(c, a->taille_Q);
    printf("%d \n", nb_classes);
    affiche_tableau(c,a->taille_Q);

    int* cc = raffiner(a,c);
    affiche_tableau(cc,a->taille_Q);

    int* n = nerode(a);
    affiche_tableau(n,a->taille_Q);

    free(n);
    free(cc);
    free(c);

    afd* min = minimiser(a);
    printf("min reconnait abb : %d \n", reconnu_afd(min,"abb"));
    printf("min reconnait aaa : %d \n", reconnu_afd(min,"aaa"));

    printf("initial = %d \n", min->q0);
    affiche_tableau_bool(min->finaux, min->taille_Q);
    affiche_matrice(min->delta, min->taille_Q, min->taille_Sigma);

    // liberer_afd(min);
    liberer_afd(a);

    return 0;
}
