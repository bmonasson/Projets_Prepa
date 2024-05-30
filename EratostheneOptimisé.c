#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

// tableau de booléens de taille n.
// chaque booléen est stocké sur un unique bit.

typedef struct Bit_array {
    int nb_bools;
    uint8_t* tab;
}bit_array;

// Entrée : nombre de booléens à strocker
// Sortie : renvoie le tableau dans une structure passée par valeur
// la structure contient l'adresse du tableau : le tableau est donc mutable
bit_array* bit_array_create(int n){
    bit_array* bit_a = malloc(sizeof(bit_array));
    uint8_t* tab = malloc(sizeof(uint8_t) * (n / 8 + 1));
    bit_a->nb_bools = n;
    bit_a->tab = tab;
    return bit_a;
}

void bit_array_delete(bit_array* a){
    free(a->tab);
    free(a);
}

int bit_array_len(bit_array* a){
    return a->nb_bools;
}

// Entrée : i entre 0 et n-1, n étant la taille de a
// Sortie : a[i] sous forme de booléen (stocké sur 8 bits)
bool bit_array_get(bit_array* a, int i){
    int i_case = i/8;
    int i_bool = i%8;
    uint8_t case_ = a->tab[i_case];
    case_ = case_ << i_bool;
    case_ = case_ >> 7;
    return case_ ; 
}

void bit_array_set(bit_array* a, int i, bool b){
    uint8_t c = a->tab[i/8];

    uint8_t x = 1;
    x = x << (7-(i%8));
    if (b){
        c = c|x;
    }
    else {
        x = ~x;
        c = c&x;
    }
    a->tab[i/8] = c;
}



int indice(int x){
    return x-1;
}

void eratosthene(bit_array* a){
    for(int i=0; i< a->nb_bools; i++){
        bit_array_set(a, i, true);
    }
    for (int i=2; i <= sqrt(a->nb_bools); i++){
        if(bit_array_get(a, indice(i))){ //si i est un premier
            for(int j = i*i; j<= a->nb_bools; j=j+i){
                bit_array_set(a, indice(j), false); //alors tous les multiples de a ne sont pas premiers
            }

        }
    }
}

int main() {
    int x;
    scanf("%d", &x);
    bit_array* a = bit_array_create(x);
    eratosthene(a);
    
    int count = -1;
    for (int i=0; i<x; i++){
        if (bit_array_get(a, i)){
            //printf("%d est premier \n", i+1);
            count ++;
        }
    }
    printf("%d\n", count);

    return 0;
}

