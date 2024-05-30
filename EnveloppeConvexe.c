#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

//IMPLEMENTATION DES PILES (correction donnée sur cahier de prépa): 
// Piles d'entiers mutables

struct Maillon {
    int val;
    struct Maillon* suivant;
};

typedef struct Maillon liste;

void liste_liberer(liste* l){
    while (l!=NULL){
        liste* m = l;
        l = l->suivant;
        free(m);
    }
}

// Renvoie la liste obtenue en ajoutant x en tête de l
liste* liste_cons(int x, liste* l){
    liste* m = malloc(sizeof(liste));
    m->val = x;
    m->suivant = l;
    return m;
}

struct Pile {
    liste* sommet;
};

typedef struct Pile pile;

pile* pile_creer() {
    pile* p = malloc(sizeof(pile));
    p->sommet = NULL;
    return p;
}

bool pile_estVide(pile* p) {
    return p->sommet == NULL;
}

// échoue si la pile est vide
int pile_regard(pile* p) {
    return p->sommet->val;
}

void pile_empiler(int x, pile* p){
    liste* m = liste_cons(x, p->sommet);
    p->sommet = m;
}

// échoue si la pile est vide
int pile_depiler(pile* p) {
    liste* m = p->sommet;
    int x = m->val;
    p->sommet = m->suivant;
    free(m);
    return x;
}

void pile_liberer(pile* p) {
    liste_liberer(p->sommet);
    free(p);
}

void pile_afficher(pile* p) {
    printf("Pile :\n[");
    for (liste* l = p->sommet; l != NULL; l = l->suivant){
        printf("%d",l->val);
        if (l->suivant != NULL) {
            printf(", ");
        }
    }
    printf("]\n");
}

















struct point {
    int x;
    int y;
};

typedef struct point point;


//Fonction supplémentaire utilisée pour print des listes d'entiers
void print_lst(int* lst, int n){
    printf("[%d, ", lst[0]);
    for(int i=1; i<n-1; i++){
        printf(" %d, ", lst[i]);
    }
    printf(" %d]\n", lst[n-1]);
}



//A. Préliminaires

int plus_bas(point* tab, int n){
    if (n==0){ //pour ne pas avoir de seg fault si le tableau est vide
        return -1;
    }
    int indice_min = 0;
    for (int i=1; i<n; i++){
        if ((tab[i].y < tab[indice_min].y) || (tab[i].y == tab[indice_min].y && tab[i].x < tab[indice_min].x)){
            indice_min = i;
        }


    }
    return indice_min;
}

int orient(point* tab, int i, int j, int k){
    point pi = tab[i];
    point pj = tab[j];
    point pk = tab[k];
    float aire_signee = ((pk.y - pi.y)*(pj.x - pi.x) - (pk.x - pi.x)*(pj.y - pi.y));
    if (aire_signee > 0){
        return 1;
    }
    if (aire_signee < 0){
        return -1;
    }
    return 0;
}


//B. Algorithme du papier cadeau (Jarvis)

int prochain_point(point* tab, int n, int i){

    int max = 0;
    for (int k=0; k<n; k++){
        if (k != i ){
            if (orient(tab, i, max, k) <= 0){
                max = k;
            }
        }
    }
    return max;
}


int conv_jarvis(point* tab, int n, int * env){
    int i = 0;
    int debut = plus_bas(tab, n);
    env[0] = debut;
    int pp = prochain_point(tab, n, debut);
    while (pp!=debut && i < n){
        i++;
        env[i] = pp;
        pp = prochain_point(tab, n, pp);
    }
    return i+1;
}


//C. ALGORITHME DE BALAYAGE (Graham / Andrew)

/*
VERSION AVEC LES FONCTIONS DONNEES DANS LE SUJET
void maj_es(point* tab, stack es, int i){
    int der = pop(es);

    while (!is_empty(es)){
        int av_der = top(es);
        int or = orient(tab, i, der, av_der);
        if (or>0){
            push(der, es);
            push(i, es);
            return;
        }

        der = pop(es);
    }

    push(der, es);
    push(i, es);
    return;

}


void maj_ei(point* tab, stack ei, int i){
    int der = pop(ei);

    while (!is_empty(ei)){
        int av_der = top(ei);
        int or = orient(tab, i, der, av_der);
        if (or<0){
            push(der, ei);
            push(i, ei);
            return;
        }

        der = pop(ei);
    }

    push(der, ei);
    push(i, ei);
    return;
}


stack conv_graham(point* tab, int n){
    stack es = new_stack();
    push(tab[0], es);
    for (int i=1; i<n; i++){
        maj_es(tab, es, i);
    }

    stack ei = new_stack();
    push(tab[0], ei);
    for (int i=1; i<n; i++){
        maj_ei(tab, ei, i);
    }

    //On vide enfin es dans ei on faisant attention à ne pas comptabiliser tab[0] une deuxième fois
    while (top(es)!=tab[0]){
        push(pop(es), ei);
    }

    return ei;
}
*/


//Version avec l'implémentation des piles du cours donnée sur cahier de prépa
void maj_es(point* tab, pile* es, int i){
    int der = pile_depiler(es);

    while (!pile_estVide(es)){
        int av_der = pile_regard(es);
        int or = orient(tab, i, der, av_der);
        if (or>0){
            pile_empiler(der, es);
            pile_empiler(i, es);
            return;
        }

        der = pile_depiler(es);
    }

    pile_empiler(der, es);
    pile_empiler(i, es);
    return;

}


void maj_ei(point* tab, pile* ei, int i){
    int der = pile_depiler(ei);

    while (!pile_estVide(ei)){
        int av_der = pile_regard(ei);
        int or = orient(tab, i, der, av_der);
        if (or<0){
            pile_empiler(der, ei);
            pile_empiler(i, ei);
            return;
        }

        der = pile_depiler(ei);
    }

    pile_empiler(der, ei);
    pile_empiler(i, ei);
    return;
}


pile* conv_graham(point* tab, int n){
    pile* es = pile_creer();
    pile_empiler(0, es);
    pile* ei = pile_creer();
    pile_empiler(0, ei);
    for (int i=1; i<n; i++){
        maj_es(tab, es, i);
        maj_ei(tab, ei, i);
    }


    //On vide enfin es dans ei on faisant attention à ne pas comptabiliser tab[0] une deuxième fois
    if (pile_regard(ei)!= 0){
        pile_depiler(ei);
    }
    while (pile_regard(es)!=0){
        pile_empiler(pile_depiler(es), ei);
    }

    return ei;
}




int main(){
    point tab [] = { 
        { 0, 0 }, { 1 , 4 }, { 1 , 8 }, { 4, 1 } , { 4, 4 } , { 5, 6 }, { 5, 9 }, { 7 , -1 } , { 7, 2 }, { 8 , 5 }, { 11 , 6 }, { 13 , 1 }
    };
    int env_jarvis [] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};

    printf("Question 2. \nplus_bas(tab, 12) = %d\n\n", plus_bas(tab, 12));
    printf("Question 3. \n1. %d\n2. %d\n\n", orient(tab,0,3,4), orient(tab,8,9,10));

   
    printf("Algorithme du paquet cadeau (Jarvis)\n");
    printf("Nombre de points de l'enveloppe convexe : %d\n", conv_jarvis(tab, 12, env_jarvis));
    printf("Liste des indices des points de l'enveloppe convexe : ");
    print_lst(env_jarvis, 12);

    printf("\nAlgorithme de balayage (Graham / Andrew)\n");
    pile* env_graham = conv_graham(tab, 12);
    pile_afficher(env_graham);

}