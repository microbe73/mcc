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
    return a1 + a2 + a3;
}
int main() {
    int r1 = 0;
    int r2 = 0;
    int r3 = 0;
    int x = drawBoard(r1, r2, r3);
    int userInput = getchar() - 48;
    if (userInput == 1){
        r1 = r1 + 1;
    }
    x = drawBoard(r1, r2, r3);
    return 0;
}
