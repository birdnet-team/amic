// Initialize and set up the audio player
function setUpAudioPlayer(playerId, iconId, imageId) {
  const player = document.getElementById(playerId);
  const icon = document.getElementById(iconId);
  const speciesImage = document.getElementById(imageId);

  if (!player || !icon || !speciesImage) {
    console.log('One of the elements is missing:', playerId, iconId, imageId);
    return;
  }

  speciesImage.addEventListener('click', function () {
    if (player.paused) {
      player.play();
      icon.innerHTML = '<i class="bi bi-pause-fill"></i>';
    } else {
      player.pause();
      icon.innerHTML = '<i class="bi bi-play-fill"></i>';
    }
  });

  player.addEventListener('ended', function () {
    icon.innerHTML = '<i class="bi bi-play-fill"></i>';
  });

  console.log('Audio player set up for:', playerId, iconId, imageId);
}

// Initialize all audio players
function initializePlayers() {
  const players = document.querySelectorAll('.detection_sound_image');
  for (const player of players) {
    const playerId = player.querySelector('audio').id;
    const iconId = player.querySelector('div.overlay_icon').id;
    const imageId = player.querySelector('img').id;
    console.log('Found player:', playerId); // Log player ID
    setUpAudioPlayer(playerId, iconId, imageId);
  }
  console.log("Players initialized!");
}

// Check initialization and ensure all elements are present before initializing players
function checkInitialization(targetLength) {
  const currentLength = document.querySelectorAll('.detection_sound_image').length;
  console.log(`Checking initialization: currentLength = ${currentLength}, targetLength = ${targetLength}`);

  if (currentLength >= targetLength) {
    initializePlayers();
    console.log("All elements are present. Players initialized!");
  } else {
    requestAnimationFrame(() => checkInitialization(targetLength));
    console.log("Players not found, rechecking...");
  }
}

$(document).ready(function() {
  console.log("Document ready, awaiting initialization signal...");
});

$(document).ready(function() {
  Shiny.addCustomMessageHandler('newCardsAdded', function(message) {
    console.log("Custom message received:", message);
    const targetLength = message.targetLength;
    checkInitialization(targetLength); // Use the checkInitialization function
    console.log("New cards added, initialization checking started!");
  });
});
