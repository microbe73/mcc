int putchar(int c);
int getchar();

int drawChar(int c){
    if (c == 0) {
        putchar(95);
    }
    else if (c == 1){
        putchar(120);
    }
    else if (c == 2) {
        putchar(111);
    }
    else{
        return -1;
    }
    return 0;
}
int drawRow(int row){
    int c1 = 3 & row;
    int a1 = drawChar(c1);
    int c2 = 3 & (row >> 2);
    int a2 = drawChar(c2);
    int c3 = 3 & (row >> 4);
    int a3 = drawChar(c3);
    return a1 + a2 + a3;
}
int drawBoard(int row1, int row2, int row3){
    int a1 = drawRow(row1);
    putchar(10);
    int a2 = drawRow(row2);
    putchar(10);
    int a3 = drawRow(row3);
    putchar(10);
    putchar(10);
    return a1 + a2 + a3;
}
int updateRow(int input, int turn){
    if (turn == 1){
        if (input == 1){
            return 1;
        }
        else if (input == 2){
            return 4;
        }
        else
            return 16;
    }
    else{
        if (input == 1)
            return 2;
        else if (input == 2){
            return 8;
        }
        else
            return 32;

    }
    return 0;
}
int main() {
    int r1 = 0;
    int r2 = 0;
    int r3 = 0;
    int x = 0;
    int n = 1;
    int won = 0;
    while (n < 10 && !won){
        x = drawBoard(r1, r2, r3);
        int input = getchar() - 48;
        if (input < 1){
            putchar(input);
            putchar(69);
            putchar(10);
            n = n - 1;
        }
        else if (input <= 3){
            r1 = r1 + updateRow(input, n % 2);
        }
        else if (input <= 6){
            r2 = r2 + updateRow(input - 3, n % 2);
        }
        else if (input <= 9){
            r3 = r3 + updateRow(input - 6, n % 2);
        }
        else{
            putchar(input);
            putchar(69);
            putchar(10);
            n = n - 1;
        }
        n = n + 1;
        int discard = getchar();
    }
    x = drawBoard(r1, r2, r3);
    return 0;
}
