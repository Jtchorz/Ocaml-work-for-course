int main() {
    int x = 0;
    int y = 0;
    while (x < 10) {
        while (y < 7) {
            y++;
        }
        x = x + y;
    }
    return x + y;
}