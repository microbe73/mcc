int putchar(int c);
int getchar();

int drawChar(int c){
    if (c == 0) {
        putchar(95);
    }
    else if (c == 1){
        putchar(120);
    }
    else if (c == 2){
        putchar(111);
    }
    return 0;
}
int drawRow(int row){
    int c1 = 3 & row;
    drawChar(c1);
    int c2 = 3 & (row >> 2);
    drawChar(c2);
    int c3 = 3 & (row >> 4);
    drawChar(c3);
    return 0;
}
int drawBoard(int row1, int row2, int row3){
    drawRow(row1);
    putchar(10);
    drawRow(row2);
    putchar(10);
    drawRow(row3);
    putchar(10);
    return 0;
}
int main() {
    int x = drawBoard(0, 0, 0);
    return 0;
}
