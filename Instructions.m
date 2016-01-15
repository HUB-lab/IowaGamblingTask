function Instructions(Window)

    
    inst_charwrap = 60;

    inst1 = ['In front of you on the screen, there are four decks of cards A, B, C, and D.\n\n' ...
        'I want you to select one card at a time, by clicking on the card, from any deck you choose.\n\n' ...
        'Press any button to continue the instructions.'];
    
    inst2 = ['Each time you select a card from a deck, the color of the card turns red or black, \n\n' ...
        'and the computer will tell you that you won some money. I won''t tell you how much money you will win. \n\n' ...
        'You will find out along the way. Every time you win, the green bar gets longer.'];
        'Press any button to continue the instructions.'];
    
    inst3 = ['Every so often, however, when you click on a card, the computer tells you that you won some money,\n\n' ...
        'but then it says that you also lost some money. I won''t tell you when you will lose or how much you will lose.\n\n' ...
        'You will find out along the way. Every time you lose, the green bar gets shorter.\n\n' ...
        'Press any button to continue the instructions.'];
    
    inst4 = ['You are absolutely free to switch from one deck to another any time you wish.\n\n' ...
        'The goal of the game is to win as much money as possible and, if you find yourself unable to win,\n\n' ...
        'make sure you avoid losing money as much as possible.\n\n' ...
        'Press any button to continue the instructions.'];

    inst5 = ['I won''t tell you for how long the game will continue. You must keep on playing until the computer stops.\n\n' ...
        'You will get this $2000 credit (see the green bar) to start the game. At the end, we will see how much you won or lost.\n\n' ...
        'The red bar here is a reminder of how much money you borrowed to play the game.\n\n' ...
        'Press any button to continue the instructions.'];

    inst6 = ['It is important to know that the colors of the cards are irrelevant in this game.\n\n' ...
        'The computer does not make you lose money at random. However, there is no way for you to figure out \n\n' ....
        'when the computer will make you lose. All I can say is that you may find yourself losing money on all of\n\n' ...
        'the decks, but some decks will make you lose more than others. You can win if you stay away from the worst decks.'];
    
    all_inst = {inst1,inst2,inst3,inst4,inst5,inst6};
    
    Screen('TextSize',wPtr,30);
    for i = 1:4
        DrawFormattedText(wPtr,all_inst{i},'center','center',225,inst_charwrap);
        Screen('Flip',wPtr);
        WaitSecs(0.4);
        KbWait(-1);
    end
    

end