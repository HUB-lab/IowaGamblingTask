function [images] = IGTSetupImages(w)

	ShowCursor;
    
    [images.xres,images.yres] = Screen('windowsize',w);
    xcenter = images.xres/2;
    ycenter = images.yres/2;

    KbName('UnifyKeyNames');

    Screen('TextFont',w,'Helvetica');
    Screen('TextSize',w,20);
    Screen('TextStyle',w,1);
    Screen('TextColor',w,[255 255 255]);
    images.wrap = 100;

    %% prepare images and textures
    %load deck, selecteddeck and background images and convert to textures
    img = imread('.\images\deck.jpg');
    tablebackground = imread('.\images\GreenFeltBackground.jpg');
    images.deck_width = 144;
    images.deck_height = 206;
    images.decktexture = Screen('MakeTexture', w, img);
    img = imread('.\images\deckon.jpg');
    images.selecteddecktexture = Screen('MakeTexture', w, img);
    % calculate positions
    images.position.deck{1} = [images.xres/2-3*images.xres/16-2*images.deck_width, images.yres/5, images.xres/2-3*images.xres/16-images.deck_width, images.yres/5+images.deck_height];
    images.position.deck{2} = [images.xres/2-images.xres/16-images.deck_width, images.yres/5, images.xres/2-images.xres/16, images.yres/5+images.deck_height];
    images.position.deck{3} = [images.xres/2+images.xres/16, images.yres/5, images.xres/2+images.xres/16+images.deck_width, images.yres/5+images.deck_height];
    images.position.deck{4} = [images.xres/2+3*images.xres/16+images.deck_width, images.yres/5,images.xres/2+3*images.xres/16+2*images.deck_width, images.yres/5+images.deck_height];
    images.position.label{1} = [images.xres/2-3*images.xres/16-2*images.deck_width, 0.5*(images.yres/5), images.xres/2-3*images.xres/16-images.deck_width, 0.9*(images.yres/5)];
    images.position.label{2} = [images.xres/2-images.xres/16-images.deck_width, 0.5*(images.yres/5), images.xres/2-images.xres/16, 0.9*(images.yres/5)];
    images.position.label{3} = [images.xres/2+images.xres/16, 0.5*(images.yres/5), images.xres/2+images.xres/16+images.deck_width, 0.9*(images.yres/5)];
    images.position.label{4} = [images.xres/2+3*images.xres/16+images.deck_width, 0.5*(images.yres/5),images.xres/2+3*images.xres/16+2*images.deck_width, 0.9*(images.yres/5)];
    images.label{1} = 'A'; images.label{2} = 'B'; images.label{3} = 'C'; images.label{4} = 'D';
    images.tablebackground = Screen(w,'MakeTexture',tablebackground );
    for deck = 1:4
        images.deck{1} = Screen(w,'MakeTexture',images.position.deck{1});
    end
    images.position.colorbar = [images.xres/3, 0.8*images.yres, 2.2*(images.xres/3), 0.9*images.yres];
    img = imread('.\images\colorbar.jpg');
    images.colorbartexture = Screen('MakeTexture', w, img);
    images.earnings = 2000.00;
    
    

end
